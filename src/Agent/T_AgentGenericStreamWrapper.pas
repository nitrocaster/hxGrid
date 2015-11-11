//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_AgentGenericStreamWrapper;

//this is read-only IGenericStream wrapper around reference to IGenericStream
//Calls Release on original object()

interface
uses I_GenericStream, T_GenericStreamRO;

type  TAgentGenericStreamWrapper = class(TGenericStreamRO)
  private
    stream: pointer;

  public
   constructor Create(stream :pointer);  //pass pointer to IGenericStream

   destructor Destroy;

 end;

implementation

//==============================================================================
//==============================================================================
constructor TAgentGenericStreamWrapper.Create(stream :pointer);
begin
 self.stream:=stream;

 IGenericStream(stream)._AddRef();

 inherited Create(IGenericStream(stream).GetBasePointer(),IGenericStream(stream).GetLength());
end;

//==============================================================================
//==============================================================================
destructor TAgentGenericStreamWrapper.Destroy;
begin
 IGenericStream(stream)._Release();
end;

end.
