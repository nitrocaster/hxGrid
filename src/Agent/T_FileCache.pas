//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

{$I-}
unit T_FileCache;

interface
uses Windows, Classes, T_AgentPeer, SyncObjs, safefpu;


//==============================================================================
// class TFileCache
//==============================================================================
//Класс, который хранит кеш файлов, включая dll, для текущей сессии
type TFileCache = class
  public

   constructor Create(peer: TAgentPeer; parent: TObject; sessionId: DWORD);
   destructor Destroy(); override;

   function GetFile(const fileName: string): TMemoryStream;

   function GetDLLHandle(const fileName: string): THANDLE;

   procedure EndSession();

   procedure GetSessionCacheDirectory(path:pchar);

  private

   //здесь храним DLL
   //f.e. "C:\temp\192.168.0.1\03FE4543\"
   cacheDirectory: String;

   list: TList;

   peer: TAgentPeer;
   parent: TObject; //TAgent;
   sessionId: DWORD;

   cs: TCriticalSection;

   function GetFileIndex(const fileName: string): integer;
   procedure LoadDLL(index: integer);
   function CreateLocalFile(index: integer): boolean;
   function CreateLocalFileName(const fname: string): string;
   procedure PurgeCache();
 end;

implementation
uses SysUtils, unit1, hxgridcommon, StrUtils,
     IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
     T_Agent, I_GridUser, crc32;

//=================================
// TListItem
//=================================
type TListItem = record
  //имя на клиенте
  fileName  : string;

  //имя локально в кеше, если сохранен
  localFileName : string;

  //содержимое
  stream: TMemoryStream;

  //DLL handle, если уже загружен
  dllHandle : THANDLE;
 end;

 type PListItem = ^TListItem;

//==============================================================================
//==============================================================================
constructor TFileCache.Create(peer: TAgentPeer; parent: TObject; sessionId : DWORD);
begin
 self.peer:=peer;
 self.parent:=parent;
 self.sessionId:=sessionId;
 cs:=TCriticalSection.Create();
 list:=TList.Create();
 cacheDirectory:=(parent as TAgent).baseCachePath+
                 peer.GetUserIP()+'\'
                 +inttohex(sessionId,8)+'\';
 ioresult;
end;

//==============================================================================
//==============================================================================
destructor TFileCache.Destroy();
begin
 PurgeCache();
 list.Destroy();
 cs.Destroy();
end;

//==============================================================================
//==============================================================================
//return contents of a file.
//request from user, if required
//called from worker thread to:
//1) get DLL to execute (throught RunTask())
//2) get any file from user (throught GetFile)
//blocking method
//fileName is user file name
//can return -1 if not found on user
//вызывается из task(Worker thread) или Worker thread
function TFileCache.GetFileIndex(const fileName: string): integer;
var
 i: integer;
 fname: string;
 stream : TMemoryStream;
 item : ^TListItem;
begin
 cs.Enter();

 fname:=AnsiUpperCase(fileName);

 //try to find file in cache
 for i:=0 to list.count-1 do
  begin
   if (TListItem(list[i]^).fileName = fname) then
    begin
     (parent as TAgent).DebugWrite('FILECACHE: File %s reused from cache.',[fileName]);
     result:=i;
     cs.Leave();
     exit;
    end
  end;

 (parent as TAgent).DebugWrite('FILECACHE: Requesting file %s from user.',[fileName]);

 stream:=peer.RequestFile(fName);

 if stream=nil then
  begin
   (parent as TAgent).DebugWrite('FILECACHE: Request for file %s failed - NULL.',[fileName]);
   result:=-1;
   cs.Leave();
   exit;
  end;

 new(item);
 item.fileName:=fname;
 item.localFileName:='';
 item.stream:=stream;
 item.dllHandle:=0;

 list.add(item);

 result:=list.count-1;

 cs.Leave();
end;

//==============================================================================
//==============================================================================
//can return NULL if not found on user
//вызывается из task(Worker thread)
function TFileCache.GetFile(const fileName: string): TMemoryStream;
var
 index: integer;
begin
 cs.Enter();

 index:=GetFileIndex(fileName);
 if index=-1 then
  begin
   (parent as TAgent).DebugWrite('FILECACHE: File %s not found on user.',[fileName]);
   result:=nil;
  end
   else
  begin
   (parent as TAgent).DebugWrite('FILECACHE: File %s provided to worker thread.',[fileName]);
   result:=TListItem(list[index]^).stream;
  end;
 cs.Leave();
end;

//==============================================================================
//==============================================================================
//вызывается из Worker thread
//строка может указывать несколько файлов, через запятую без пробелов,
//например: c:\task.dll,c:\task_dependent.dll
//Внимание:если имя файла содержит пробелы, имя файла должно быть заключено в ""
//например: c:\task.dll,"c:\Program Files\task_dependent.dll"
//dllHandle возвращается для первого файла в списке
function TFileCache.GetDLLHandle(const fileName: string) : THANDLE;
var
 index: integer;
 filelist: TStringList;
 i: integer;
begin
 cs.Enter();

  if pos(',',fileName)>0 then
   begin
    //parce multiple files
    filelist:=TStringList.Create();
    filelist.CommaText:=fileName;

    for i:=filelist.count-1 downto 0 do
     begin
      index:=GetFileIndex(filelist[i]);
      if index=-1 then
       begin
        (parent as TAgent).DebugWrite('FILECACHE: File %s not found on user.',[filelist[i]]);
        result:=0;
        cs.Leave();
        exit;
       end;

      //possibly this is dependent DLL
      //create local file !
      if CreateLocalFile(index)=false then
       begin
        (parent as TAgent).DebugWrite('FILECACHE: Unagle to create local file %s.',[filelist[i]]);
        result:=0;
        cs.Leave();
        exit;
       end;
     end;
    filelist.Destroy();
   end
    else
   begin
    //handle single file
    index:=GetFileIndex(fileName);

    if index=-1 then
     begin
      (parent as TAgent).DebugWrite('FILECACHE: File %s not found on user.',[fileName]);
      result:=0;
      cs.Leave();
      exit;
     end;

    if CreateLocalFile(index)=false then
     begin
      (parent as TAgent).DebugWrite('FILECACHE: Unagle to create local file %s.',[fileName]);
      result:=0;
      cs.Leave();
      exit;
     end;
   end;

 if (TListItem(list[index]^).DLLhandle=0) then LoadDLL(index);

 result:=TListItem(list[index]^).DLLhandle;

 cs.Leave();
end;

//==============================================================================
//==============================================================================
//очистить весь кеш
//вызывается из working thread(завершение, OnDisconnect)
procedure TFileCache.PurgeCache();
var
 i: integer;
 f: file;
 s:string;
begin
 cs.Enter();

 try
  //first unload all DLLs, _then_ delete files!!!
  for i:=0 to list.count-1 do
   begin
    if (TListItem(list[i]^).dllHandle<>0) then
     begin
      FreeLibrary(TListItem(list[i]^).dllHandle);
     end;
   end;

  for i:=0 to list.count-1 do
   begin
    TListItem(list[i]^).stream.Destroy();

    if (TListItem(list[i]^).localFileName<>'') then
     begin
      s:=TListItem(list[i]^).localFileName;
      AssignFile(f,s);
      Erase(f);
      ioresult;
     end;

    dispose(PListItem(list[i]));
   end;
  list.Clear();

  removedirectory(pchar((parent as TAgent).baseCachePath+peer.GetUserIP()+'\'+inttohex(sessionId,8)+'\'));
  ioresult;

  removedirectory(pchar((parent as TAgent).baseCachePath+peer.GetUserIP()+'\'));
  ioresult;

 finally
  cs.Leave();
 end;

end;

//==============================================================================
//==============================================================================
//попытаться загрузить DLL
procedure TFileCache.LoadDLL(index: integer);
begin
 cs.Enter();

 assert(TListItem(list[index]^).DLLHandle=0);

 (parent as TAgent).DebugWrite('FILECACHE: Loading library from file %s (original file - %s.',[TListItem(list[index]^).localFileName, TListItem(list[index]^).fileName]);

// TListItem(list[index]^).DLLHandle:=LoadLibrary(pchar(ExtractFilePath(TListItem(list[index]^).localFileName)+'gmpport.dll'));

 //we should use LoadLibraryEx here to allow DLL correctly map dependent DLL modules,
 //received from user.
 ///LoadLibrary() is unable to load DLL under Vista in this case.

 try
  TListItem(list[index]^).DLLHandle:=LoadLibraryEx(pchar(TListItem(list[index]^).localFileName), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
 except
 end;

 cs.Leave();
end;

//==============================================================================
//==============================================================================
//если еще не создан, создать локальный файл
function TFileCache.CreateLocalFile(index: integer): boolean;
var
 f: file;
// crc32: DWORD;
begin
 cs.Enter;

 if TListItem(list[index]^).localFileName<>'' then
  begin
   cs.Leave;
   result:=true;
   exit;
  end;

// CRC32Init(crc32);
// CRC32Update(crc32, TListItem(list[index]^).stream.memory, TListItem(list[index]^).stream.size);

 TListItem(list[index]^).localFileName:=CreateLocalFileName(TListItem(list[index]^).fileName);

 (parent as TAgent).DebugWrite('FILECACHE: Creating local file %s (original file - %s.',[TListItem(list[index]^).localFileName, TListItem(list[index]^).fileName]);

 EnsureFolderExists(TListItem(list[index]^).localFileName);
 ioresult;

 AssignFile(f,TListItem(list[index]^).localFileName);
 Rewrite(f,1);
 BlockWrite(f,TListItem(list[index]^).stream.memory^,TListItem(list[index]^).stream.size);
 CloseFile(f);

 if (ioresult<>0) then
  begin
   result:=false;
   TListItem(list[index]^).localFileName:='';
  end
   else
  begin
   result:=true;
  end; 
 
 ioresult;

 cs.Leave;
end;

//==============================================================================
//==============================================================================
//создать имя для локального файла
//возвращает полное имя, включая путь
function TFileCache.CreateLocalFileName(const fname: string): string;
begin
 result:=AnsiReplaceStr(fname, ':', '_');
 result:=AnsiReplaceStr(result, '\', '_');
 result:=AnsiReplaceStr(result, '/', '_');
 result:=cacheDirectory+result;
end;

//==============================================================================
//==============================================================================
//вызывается из working thread(завершение, OnDisconnect)
//Вызывает EndSession() из каждой DLL, которая была загружена для
//выполнения задач во время этой сесии
procedure TFileCache.EndSession();
var
 i: integer;
 s:string;
 proc2: TEndSessionProc;
begin
 for i:=0 to list.count-1 do
  begin
   if (TListItem(list[i]^).dllHandle<>0) then
    begin
     @proc2:=GetProcAddress(TListItem(list[i]^).DLLhandle, 'EndSession');
     if (@proc2=nil) then @proc2:=GetProcAddress(TListItem(list[i]^).DLLhandle, '_EndSession');

     if (@proc2<>nil) then
      begin
       try
        proc2((parent as TAgent),sessionId);
       except
        on e: Exception do
         begin
          s:='WORKERTHREAD: exception '+E.Message+'in EndSession()';
          (parent as TAgent).DebugWrite(s,[]);
         end;
       end;
      end;
    end;
  end;
end;

//==============================================================================
//==============================================================================
procedure TFileCache.GetSessionCacheDirectory(path:pchar);
begin
 strcopy(path,pchar(cacheDirectory));
end;



end.
