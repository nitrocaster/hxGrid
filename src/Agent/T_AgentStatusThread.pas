//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_AgentStatusThread;

interface
uses Windows,SyncObjs, Classes, SysUtils, hxgridcommon,
     hxScktComp, IdUDPClient, IdBaseComponent, IdComponent,
     IdUDPBase, IdUDPServer, IdStack, IdSocketHandle;

type TWin32GetSystemTimes = function(var lpIdleTime:int64; var lpKernelTime: int64; var lpUserTime: int64): Boolean; stdcall;

type TGetSystemTimes = procedure (var lpIdleTime:int64; var lpKernelTime: int64; var lpUserTime: int64) of object ;
     
//==================================
// class TAgentStatusThread
//==================================
//автономный класс,
//занимается отсылкой статуса на координатор
//практически, выполняется в собственном потоке
type TAgentStatusThread = class (TThread)
  private
   parent: TObject;

   ClientSocket: TClientSocket;
   state1 : TAgentState1;
   event1 : TEvent;  //state1 change event

   IdUDPServer1: TIdUDPServer;

   lastKernelTime, lastUserTime, lastIdleTime: int64;
   lastprocessUserTime, lastProcessKernelTime: int64;

   //incremented each time cpuusage is less then 10%
   //when reaches 10 - agent is suspended
   suspendCounter: DWORD;
   suspendstop: DWORD;

   lastCPUUsageUpdate:  DWORD; //gettickcount()

   GetSystemTimes: TGetSystemTimes;

   pWin32GetSystemTimes : TWin32GetSystemTimes;

   UpdatePeriod : DWORD;

   procedure GetCPUFrequency();
   procedure GetCPUCount();
   procedure GetComputerName();
   procedure UpdateCPUUsage();
   procedure Broadcast();
   procedure UpdateSuspendStatus();

   procedure IdUDPServer1UDPRead(Sender: TObject; AData: TStream;
      ABinding: TIdSocketHandle);

   procedure SetupGetSystemTimes();   
   procedure GetSystemTimesXP(var lpIdleTime:int64; var lpKernelTime: int64; var lpUserTime: int64);
   procedure GetSystemTimes2000(var lpIdleTime:int64; var lpKernelTime: int64; var lpUserTime: int64);
      

  public

   CPUAvail         : DWORD; //percent
   CPUUsageByAgent  : DWORD; //percent
   AvailMem         : DWORD; // MB
   CPUFrequency     : single; //GHz
   CPUCount         : DWORD;
   ComputerName     : string;

   constructor Create(parentAgent: TObject);
   destructor Destroy(); override;
   procedure Execute(); override;
   procedure Terminate();
   function GetState(): TAgentState1;
   function GetCPUAndMemoryStatus(): string;
 end;

implementation
uses Unit1, T_Agent, HTTPApp, StrUtils, Registry, T_AgentThreadPool;


//==============================================================================
//constructor TAgentStatusThread.Create();
//==============================================================================
constructor TAgentStatusThread.Create(parentAgent: TObject);
var
 lastCreationTime, lastExitTime : FILETIME;
begin
 self.parent:=parentAgent;

 SetupGetSystemTimes();

 lastCPUUsageUpdate:=GetTickCount();

 event1:=TEvent.Create(nil,false,false,'');

 state1:=AS1_BROADCAST_TO_COORDINATOR;

 GetSystemTimes(lastIdleTime, lastKernelTime, lastUserTime);
 GetProcessTimes(GetCurrentProcess(), lastCreationTime, lastExitTime, FILETIME((@lastProcessKernelTime)^), FILETIME((@lastProcessUserTime)^));
 CPUAvail:=0;
 CPUUsageByAgent:=0;

 suspendStop:=GetTickCount();
 suspendCounter:=0;

 try
  IdUDPServer1:=TIdUDPServer.Create(nil);
  IdUDPServer1.defaultPort:=(parent as TAgent).settings.agent_broadcast_port;
  IdUDPServer1.OnUDPRead:=IdUDPServer1UDPRead;
  IdUDPServer1.Active:=true;
 except
  (parent as TAgent).DebugWrite('AGENT: Unable to create UDP server socket on port %d',[(parent as TAgent).settings.agent_broadcast_port]);
 end;

 inherited Create(false);
end;


//==============================================================================
//destructor TAgentStatusThread.Destroy();
//==============================================================================
destructor TAgentStatusThread.Destroy();
begin
 Terminate();
 WaitForSingleObject(Handle,INFINITE);

 IdUDPServer1.Destroy();
 
 event1.Destroy();
end;

//==============================================================================
//==============================================================================
procedure TAgentStatusThread.SetupGetSystemTimes();
var
 dllhandle: HMODULE;
begin
 dllhandle:=LoadLibrary('Kernel32.dll');

 pWin32GetSystemTimes:=GetProcAddress(dllhandle,'GetSystemTimes');
// pWin32GetSystemTimes:=nil;

 FreeLibrary(dllhandle);  //kernel32 will not be unloaded

 if @pWin32GetSystemTimes=nil then
  begin
   GetSystemTimes:=GetSystemTimes2000;
   UpdatePeriod:=2000;
  end
   else
  begin
   GetSystemTimes:=GetSystemTimesXP;
   UpdatePeriod:=300;
  end;
end;

//==============================================================================
//==============================================================================
procedure TAgentStatusThread.GetSystemTimesXP(var lpIdleTime:int64; var lpKernelTime: int64; var lpUserTime: int64);
begin
 pWin32GetSystemTimes(lpIdleTime, lpKernelTime, lpUserTime);
end;

//==============================================================================
//==============================================================================
//emulate GetSystemTimes for Win2000
procedure TAgentStatusThread.GetSystemTimes2000(var lpIdleTime:int64; var lpKernelTime: int64; var lpUserTime: int64);
begin
 lpKernelTime:=0;

 if GetCurrentThreadId()=form1.mainThreadId then
  begin
   Form1.UpdateCPUUsageWMI();
  end
   else
  begin
   Synchronize(Form1.UpdateCPUUsageWMI);
  end;

 lpUserTime:=form1.lpUserTime;
 lpIdleTime:=form1.lpTimer100ns;
end;

//==============================================================================
//procedure TAgentStatusThread.Execute();
//==============================================================================
procedure TAgentStatusThread.Execute();
var
 locale: TFormatSettings;
begin
 SetThreadName('Status thread');

 GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, locale);

 ClientSocket:=TClientSocket.Create(nil);
 ClientSocket.Port:=(parent as TAgent).settings.coordinator_port;
 ClientSocket.ClientType:=ctBlocking;

 while (not Terminated) do
  begin
   Sleep(100);
   
   UpdateCPUUsage();

   case state1 of
    AS1_BROADCAST_TO_COORDINATOR:
     begin
      (parent as TAgent).settingsCS.Enter();

      if (parent as TAgent).settings.coordinator_ip<>'' then
       begin
       (parent as TAgent).settingsCS.Leave();

        state1:=AS1_CONNECT_TO_COORDINATOR;
       end
        else
       begin
        (parent as TAgent).settingsCS.Leave();
        Broadcast();

        Event1.WaitFor(AGENT_COORDINATOR_RECONNECT_TIMEOUT);

        state1:=AS1_BROADCAST_TO_COORDINATOR;
       end;
     end;

    //--------------------------------------------
    AS1_CONNECT_TO_COORDINATOR:
     begin
      try
       ClientSocket.Host:=(parent as TAgent).settings.coordinator_ip;
       ClientSocket.Open();
       state1:=AS1_UPDATE_STATUS;
      except
       if ((parent as TAgent).settings.allowDiscardCoordinatorIp) then
        begin
         (parent as TAgent).settingsCS.Enter();
         (parent as TAgent).settings.coordinator_ip:='';
         (parent as TAgent).settingsCS.Leave();
        end;
       state1:=AS1_BROADCAST_TO_COORDINATOR;
      end;
     end;

    AS1_CONNECT_TO_COORDINATOR_PAUSE:
     begin
      state1:=AS1_CONNECT_TO_COORDINATOR;
      Event1.WaitFor(AGENT_COORDINATOR_RECONNECT_TIMEOUT);
     end;

    //--------------------------------------------
    AS1_UPDATE_STATUS:
     begin
      try
       if SocketWriteString(ClientSocket.Socket,
                            'UPDATE_STATUS' + ' '+
                            inttostr(CURRENT_AGENT_VERSION)+' '+
                            HTTPEncode(ComputerName) + ' '+
                            AnsiReplaceStr(formatfloat('0.000',CPUFrequency,locale),',','.') + ' '+
                            inttostr(CPUAvail)+' '+
                            inttostr(CPUUsageByAgent)+' '+
                            inttostr(int64(AvailMem))+' '+
                            HTTPEncode((parent as TAgent).GetStatus())+' '+
                            inttostr(CPUCount)+' '+
                            inttostr((parent as TAgent).settings.poolSize),
// todo: send agent ports to coordinator, and from coordinator to user
// todo: send user ports to agents
//                            inttostr((parent as TAgent).settings.bind_port),
                            10000,
                            (parent as TAgent).settings.enableDebugOutput) = false then
        begin
         ClientSocket.Close();
         raise EInOutError.Create('');
        end;
       state1:=AS1_UPDATE_STATUS_PAUSE;
      except
       on E: Exception do
        begin
         if (ClientSocket.Socket.Connected=false) then
          begin
           state1:=AS1_CONNECT_TO_COORDINATOR_PAUSE;
          end;
        end;
      end;

     end;

    //--------------------------------------------
    AS1_UPDATE_STATUS_PAUSE:
     begin
      Event1.WaitFor(AGENT_UPDATE_STATUS_PERIOD);
      state1:=AS1_UPDATE_STATUS;
     end;

   end;

   UpdateSuspendStatus();

  end;

 ClientSocket.Close();
 ClientSocket.Destroy();
end;

//==============================================================================
//procedure TAgentStatusThread.Terminate();
//==============================================================================
procedure TAgentStatusThread.Terminate();
begin
 Event1.SetEvent();

 inherited Terminate();
end;

//==============================================================================
//==============================================================================
procedure TAgentStatusThread.UpdateCPUUsage();
var
 IdleTime:int64;
 KernelTime: int64;
 UserTime: int64;

 i1: int64;
 i2: int64;
 i3: int64;

 processUserTime, processKernelTime: int64;
 _processKernelTime : FileTime absolute processKernelTime;
 _processUserTime : FileTime absolute processUserTime;
 CreationTime, ExitTime: FileTime;

 ms: TMEMORYSTATUS;

 d: DWORD;

begin
 d:=GetTickCount();
 if (d-lastCPUUsageUpdate<UpdatePeriod) then exit;
 lastCPUUsageUpdate:=d;

 GetCPUCount();

 GetSystemTimes(IdleTime, KernelTime, UserTime);

 if (@pWin32GetSystemTimes<>nil) then
  begin
   i1:=(userTime-lastUserTime) + (kernelTime-lastKernelTime);
   i2:=(IdleTime-lastIdleTime);
   i3:=i1 div 100;
   if i3<1 then i3:=1;

   i1:=i1-i2;
   i1:=i1 div i3;

   CPUAvail:=(100-i1);
  end
   else
  begin
//   (parent as TAgent).DebugWrite('userTime-lastUserTime= %d - %d = %d',[userTime,LastUserTime, userTime-lastUSerTime]);
//   (parent as TAgent).DebugWrite('idleTime-lastIdleTime= %d - %d = %d',[idleTime,LastIdleTime, idleTime-lastIdleTime]);

   i1:=userTime-lastUserTime;
   i2:=IdleTime-lastIdleTime;

   i3:=i2 div 100;
   if i3<1 then i3:=1;

   i1:=i1 div i3;

   CPUAvail:=i1;

   i3:=i3*CPUCount;
   if i3<1 then i3:=1;
  end;

 lastIdleTime:=IdleTime;
 lastKernelTime:=KernelTime;
 lastUserTime:=UserTime;

 GetProcessTimes(GetCurrentProcess(), CreationTime, ExitTime, _ProcessKernelTime, _ProcessUserTime);

 i1:=(processUserTime-lastProcessUserTime) + (processKernelTime-lastProcessKernelTime);
 i1:=i1 div i3;

 CPUUsageByAgent:=i1;

 lastProcessKernelTime:=ProcessKernelTime;
 lastProcessUserTime:=ProcessUserTime;

 GlobalMemoryStatus(ms);

 AvailMem:=ms.dwAvailPhys div (1024*1024);

 GetComputerName();
 GetCPUFrequency();
end;

//==============================================================================
//==============================================================================
procedure TAgentStatusThread.GetCPUFrequency();
var
 Registry: TRegistry;
 s:string;
 i,j,code: integer;
 f: single;
begin
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey:= HKEY_LOCAL_MACHINE;
    Registry.OpenKey('HARDWARE\DESCRIPTION\System\CentralProcessor\0', False);
    CPUFrequency := Registry.ReadInteger('~MHz')/1000;
    s:= Registry.ReadString('ProcessorNameString');

    s:=AnsiUpperCase(s);

    //AMD Athlon(tm) 64 X2 Dual Core Processor 3600+
    if pos('AMD',s)>0 then
     begin
      for i:=1 to length(s) do
       begin
        j:=0;
        val(copy(s,i,1000),j,code);
        if (j>CPUFrequency*1000) and (j<CPUFrequency*1000*2) then
         begin
          CPUFrequency:=j/1000;
          break;
         end;
       end;
     end;

(*
    //>Intel(R) Core(TM)2 CPU          6300  @ 1.86GHz
    s:='Intel(R) Core(TM)2 CPU          6300  @ 1.86GHz';
    if pos('GHz',s)>0 then
     begin
      s:=AnsiReplaceStr(s,',','.');
      i:=pos('GHz',s);

      while (i>0) and (s[i] in ['0'..'9','.',',']) do dec(i);

      f:=0;
      val(copy(s,i,1000),f,code);
      if (f>CPUFrequency) and (f<CPUFrequency*2) then
       begin
        CPUFrequency:=f;
       end;
     end;
*)
  finally
    Registry.Free;
  end;
end;

//==============================================================================
//==============================================================================
procedure TAgentStatusThread.GetCPUCount();
begin
 CPUCount := hxgridcommon.GetCPUCount();
end;

//==============================================================================
//==============================================================================
procedure TAgentStatusThread.GetComputerName();
var
 Registry: TRegistry;
begin
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey:= HKEY_LOCAL_MACHINE;
    Registry.OpenKey('SYSTEM\CurrentControlSet\Control\ComputerName\ActiveComputerName', False);
    ComputerName:= Registry.ReadString('ComputerName');
  finally
    Registry.Free;
  end;
end;

//==============================================================================
//==============================================================================
function TAgentStatusThread.GetState(): TAgentState1;
begin
 result:=state1;
end;


//==============================================================================
//==============================================================================
function TAgentStatusThread.GetCPUAndMemoryStatus(): string;
var
 CPUUsedMHz, CPUAvailMHz: single;
 locale: TFormatSettings;
begin
 CPUUsedMHz:=(parent as TAgent).statusThread.CPUUsageByAgent * (parent as TAgent).statusThread.CPUFrequency/100;
 CPUAvailMHz:=(parent as TAgent).statusThread.CPUAvail * (parent as TAgent).statusThread.CPUFrequency/100;

 GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, locale);
 
 result:=AnsiReplaceStr(formatfloat('0.0',CPUUsedMHz, locale),',','.')+' '+
         AnsiReplaceStr(formatfloat('0.0',CPUAvailMHz, locale),',','.')+' '+
         inttostr((parent as TAgent).statusThread.AvailMem);
end;

//==============================================================================
//==============================================================================
//broadcast to coordinator - request echo broadcast to agent broadcast port
procedure TAgentStatusThread.Broadcast();
var
 b: byte;
 IdUDPClient1: TIdUDPClient;
begin
 IdUDPClient1:=TIdUDPClient.Create(nil);
 IdUDPClient1.Host := '255.255.255.255';
 IdUDPClient1.Port := (parent as TAgent).settings.coordinator_broadcast_port;
 IdUDPClient1.Active := true;
 IdUDPClient1.BroadcastEnabled := True;

 b:=ID_AGENT_BROADCAST;
 IdUDPClient1.SendBuffer(b, 1);

 IdUDPClient1.Destroy();
end;

//=============================================================================
//=============================================================================
procedure TAgentStatusThread.IdUDPServer1UDPRead(Sender: TObject; AData: TStream;
  ABinding: TIdSocketHandle);
var
  b: byte;
begin
 AData.Read(b, 1);

 (parent as TAgent).DebugWrite('AGENT: Got broadcast from coordinator %s: %u bytes',[ABinding.PeerIP, AData.Size]);

 if (state1<>AS1_BROADCAST_TO_COORDINATOR) then exit;

 (parent as TAgent).settingsCS.Enter();

 strcopy((parent as TAgent).settings.coordinator_ip,pchar(ABinding.PeerIp));

 (parent as TAgent).settingsCS.Leave();

 (parent as TAgent).SaveSettings();

 Event1.SetEvent();
end;

//=============================================================================
//=============================================================================
procedure TAgentStatusThread.UpdateSuspendStatus();
var
 d: dword;
begin
 if (CPUAvail+CPUUsageByAgent<MIN_FREE_CPU) then
  begin
   inc(suspendCounter);
   if suspendCounter=10 then
    begin
     suspendStop:=GetTickCount()+(parent as TAgent).settings.suspend_timeout*1000;
     suspendCounter:=0;
    end;
  end;

 if (parent as TAgent).settings.suspend_timeout=0 then
  begin
   TAgentThreadPool.SetSuspend(0);
  end
   else
  begin
   d:=GetTickCount();
   if suspendStop<d then TAgentThreadPool.SetSuspend(0)
                    else TAgentThreadPool.SetSuspend((suspendStop-d) div 1000);
  end;
end;

begin
// Service:=nil;
end.
