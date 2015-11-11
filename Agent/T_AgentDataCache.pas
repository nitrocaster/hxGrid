//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

{$I-}
unit T_AgentDataCache;

interface
uses Windows, Classes, T_AgentPeer, I_GenericStream, SyncObjs, safefpu;

//==============================================================================
// class TAgentDataCache
//==============================================================================
type TAgentDataCache = class
  public

   constructor Create(peer: TAgentPeer; parent: TObject);
   destructor Destroy(); override;

   function GetData(dataDesc: pchar; var stream:IGenericStream): HRESULT;

   //dataDesc=NULL - all data
   procedure PurgeCache(dataDesc : pchar);

  private

   list: TList;

   peer: TAgentPeer;
   parent: TObject; //TAgent;

   cs : TCriticalSection;

   procedure FreeSpaceInCache(newItemSize :DWORD);

 end;

implementation
uses SysUtils, unit1, hxgridcommon, StrUtils,
     IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
     T_Agent, T_AgentGenericStreamWrapper;

//=================================
// TListItem
//=================================
type TListItem = record
  dataDesc  : string;

  //содержимое
  stream    : IGenericStream;

  //getTickCount
  lastUsed  : DWORD;
 end;

 type PListItem = ^TListItem;

//==============================================================================
//==============================================================================
constructor TAgentDataCache.Create(peer: TAgentPeer; parent: TObject);
begin
 self.peer:=peer;
 self.parent:=parent;
 cs:=TCriticalSection.Create();
 list:=TList.Create();
end;

//==============================================================================
//==============================================================================
destructor TAgentDataCache.Destroy();
begin
 PurgeCache(nil);
 list.Destroy();
 cs.Destroy();
end;

//==============================================================================
//==============================================================================
function TAgentDataCache.GetData(dataDesc: pchar; var stream:IGenericStream): HRESULT;
var
 i: integer;
 fname: string;
 item : ^TListItem;
 strDataDesc: string;
 d: DWORD;
 tmpStream: IGenericStream;
begin
 cs.Enter();

 strdatadesc:=strpas(dataDesc);

 //try to find data in cache
 for i:=0 to list.count-1 do
  begin
   if (TListItem(list[i]^).dataDesc = strDataDesc) then
    begin
     (parent as TAgent).DebugWrite('DATACACHE: Data reused from cache.',[]);

     pointer(stream):=pointer(IGenericStream(TAgentGenericStreamWrapper.Create(pointer(TListItem(list[i]^).stream))));

     TListItem(list[i]^).lastUsed:=GetTickCount();

     result:=S_OK;
     cs.Leave();
     exit;
    end
  end;

 (parent as TAgent).DebugWrite('DATACACHE: Requesting data from user.',[]);

 peer.RequestData(dataDesc, tmpStream);

 if tmpStream=nil then
  begin
   (parent as TAgent).DebugWrite('DATA: Request for file data failed - NULL.',[]);
   result:=S_FALSE;
   cs.Leave();
   exit;
  end;

 if (IsCompressedStream(tmpStream)) then DecompressStream(tmpStream);

 new(item);
 item.dataDesc:=strDataDesc;
 item.stream:=tmpstream;  //addref
 tmpStream:=nil; //release

 FreeSpaceInCache(item.stream.GetLength());

 list.add(item);

 d:=item.stream._addref;
 d:=item.stream._release();
 assert(d=1);

 pointer(stream):=pointer(IGenericStream(TAgentGenericStreamWrapper.Create(pointer(item.stream))));

 item.lastUsed:=GetTickCount();

 result:=S_OK;

 cs.Leave();
end;

//==============================================================================
//==============================================================================
//очистить весь кеш, если dataDesc=nil
//иначе  выгрузить указанные данные
procedure TAgentDataCache.PurgeCache(dataDesc : pchar);
var
 i: integer;
 d: DWORD;
begin
 i:=0;
 while (i<list.count) do
  begin
   if ((dataDesc=nil) or (TListItem(list[i]^).dataDesc=strPas(dataDesc))) then
       begin
        d:=TListItem(list[i]^).stream._AddRef();
        d:=TListItem(list[i]^).stream._Release();
        //If task called: IAgent->GetData(), IAgent->PurgeCache(),
        //stream will have >1 refcount and will be released when
        //task calls stream->Release()
        TListItem(list[i]^).stream:=nil; //release out ref
        list.delete(i);
        dec(i);
       end;
   inc(i);
  end;
end;

//==============================================================================
//==============================================================================
//purge some items from cache if cache size + new item is bigger then specified in settings
procedure TAgentDataCache.FreeSpaceInCache(newItemSize :DWORD);
begin
 //todo
end;

end.
