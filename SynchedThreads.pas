unit SynchedThreads;

// Copyright © 1998 by Jon Shemitz, all rights reserved.
// Permission is hereby granted to freely use, modify, and
// distribute this source code PROVIDED that all six lines of
// this copyright and contact notice are included without any
// changes. Questions? Comments? Offers of work?
// mailto:jon@midnightbeach.com - http://www.midnightbeach.com

// Various modifications to Jon Shemitz original code by
// David Knaack (davidknaack@gmail.com). Mods are not intended
// to be nor should they be construed as being inteded to be
// well designed. It is more or less functional though, and
// the updates for D2009 allow for some interesting uses.
{$T+} {$hints on} {$warnings on}

interface

uses Windows, Classes, SysUtils, Forms;

// Simple threads

type
  TThreadMethod = procedure (Data: pointer) of object;
{$ifdef VER200}
  TSyncThreadProcedure = reference to procedure(Data: Pointer);
{$endif}

  TSimpleThread = class (TThread)
	 protected
		ThreadMethod: TThreadMethod;
{$ifdef VER200}
    AnonMethod: TSyncThreadProcedure;
{$endif}
		Data:         pointer;
		procedure  Execute; override;
	 public
{$ifdef VER200}
		constructor CreateSimple( CreateSuspended: boolean;
					 					 _Action: TThreadMethod; _Data: pointer ); overload;
		constructor CreateSimple( CreateSuspended: boolean;
					 					 _Action: TSyncThreadProcedure; _Data: pointer ); overload;
{$else}
    constructor CreateSimple( _Action: TThreadMethod;
                     _CreateSuspended: boolean;
                     _Data: pointer ); overload;
		constructor CreateSimple( _Action: TThreadMethod;
                     _CreateSuspended: boolean=False;
                     _FreeOnTerminate : Boolean=True;
                     _OnTerminate: TNotifyEvent=nil;
                     _Data: pointer=nil ); overload;
		constructor CreateSimple( _Action: TThreadMethod;
                     _CreateSuspended: boolean=False;
                     _FreeOnTerminate : Boolean=True;
                     _Data: pointer=nil ); overload;
{$endif}
		procedure AbortThread;
	 end;

{$ifdef VER200}
function RunInThread( Handler: TThreadMethod; Data: pointer ): TSimpleThread; overload;
function RunInThread( AnonMeth: TSyncThreadProcedure; Data: pointer; CreateSuspended:Boolean=false ): TSimpleThread; overload;
{$else}
function RunInThread( Handler: TThreadMethod;
                      CreateSuspended: Boolean=False;
                      FreeOnTerminate: Boolean=False;
                      Data: pointer=nil ): TSimpleThread; overload;
function RunInThread( Handler: TThreadMethod;
                      CreateSuspended: Boolean=False;
                      FreeOnTerminate: Boolean=false;
                      OnTerminate: TNotifyEvent=nil;
                      Data:pointer=nil ): TSimpleThread; overload;
{$endif}

// Wait threads (basic synchronization)

procedure MsgWaitForSingleObject(Handle: THandle);
function SpawnProcess(const Command: string): TProcessInformation;

type
  TWaitThread = class (TSimpleThread)
	 private
		AbortFlag: ^boolean;
		procedure Run(MsgWait: boolean);
	 public
		constructor CreateWait( _Action: TThreadMethod; _Data: pointer );
		procedure WaitFor;
		procedure MsgWaitFor;
		procedure AbortThread;
	end;

procedure WaitForThread(Handler: TThreadMethod; Data: pointer );//Blocking
procedure MsgWaitForThread(var Thread: TWaitThread;             //non-Blocking
		 						 Handler: TThreadMethod; Data: pointer );

// Stop/start threads

type
  EAbortedThread = class(Exception);
  EThreadInUse   = class(Exception);

  TStopStartThread = class (TSimpleThread)
	 private
		Event:   THandle;
		Aborted: boolean;
		procedure  Run( _Action: TThreadMethod; _Data:pointer; MsgWait: boolean );
   protected
		procedure  Execute; override;
	 public
		Waiting: boolean;
		constructor Create;
		destructor Destroy; override;
		procedure WaitFor   ( _Action: TThreadMethod; _Data:pointer );
		procedure MsgWaitFor( _Action: TThreadMethod; _Data:pointer );
		procedure AbortThread;
		end;

implementation

{ ============== TSimpleThread ============= }

function RunInThread( Handler: TThreadMethod;
                      CreateSuspended: Boolean=False;
                      FreeOnTerminate: Boolean=false;
                      Data:pointer=nil): TSimpleThread;
begin
  Result := TSimpleThread.CreateSimple(Handler, CreateSuspended, FreeOnTerminate, Data);
end;

function RunInThread( Handler: TThreadMethod;
                      CreateSuspended: Boolean=False;
                      FreeOnTerminate: Boolean=false;
                      OnTerminate: TNotifyEvent=nil;
                      Data:pointer=nil ): TSimpleThread;
begin
  Result := TSimpleThread.CreateSimple(Handler, CreateSuspended, FreeOnTerminate, OnTerminate, Data );
end;

{$ifdef VER200}
function RunInThread( AnonMeth: TSyncThreadProcedure; Data: pointer; CreateSuspended:Boolean=false): TSimpleThread;
begin
  Result := TSimpleThread.CreateSimple(CreateSuspended, AnonMeth, Data);
end;
{$endif}

constructor TSimpleThread.CreateSimple( _Action: TThreadMethod;
                     _CreateSuspended: boolean=False;
                     _FreeOnTerminate: Boolean=True;
                     _Data: pointer=nil );
begin
  ThreadMethod    := _Action; // Set these BEFORE calling
  Data            := _Data;   // inherited Create()!
  FreeOnTerminate := _FreeOnTerminate;
  inherited Create(_CreateSuspended);
end;

constructor TSimpleThread.CreateSimple( _Action: TThreadMethod;
                     _CreateSuspended: boolean;
                     _Data: pointer );
begin
  ThreadMethod := _Action;
  Data := _Data;
  inherited Create(_CreateSuspended);
end;

constructor TSimpleThread.CreateSimple( _Action: TThreadMethod;
                     _CreateSuspended: boolean=False;
                     _FreeOnTerminate: Boolean=True;
                     _OnTerminate: TNotifyEvent=nil;
                     _Data: pointer=nil );
begin
  ThreadMethod    := _Action; // Set these BEFORE calling
  Data            := _Data;   // inherited Create()!
  OnTerminate     := _OnTerminate;
  FreeOnTerminate := _FreeOnTerminate;
  inherited Create(_CreateSuspended);
end;

{$ifdef VER200}
constructor TSimpleThread.CreateSimple(CreateSuspended: boolean;
  _Action: TSyncThreadProcedure; _Data: pointer );
begin
  ThreadMethod := nil;
  AnonMethod := _Action;
  Data := _Data;
  FreeOnTerminate := true;
  inherited Create(CreateSuspended);
end;
{$endif}

procedure  TSimpleThread.Execute;
begin  //Call the procedure  pointed to by ThreadMethod
{$ifdef VER200}
  if Assigned(ThreadMethod) then
    ThreadMethod(Data)
  else
    if Assigned(AnonMethod) then
      AnonMethod(Data);
{$else}
  ThreadMethod(Data);
{$endif}
end;

procedure TSimpleThread.AbortThread;
begin
  Suspend; // Can't kill a running thread by Freeing it
  Free;    // Kills thread
end;

{ ============== Basic synchronization ============= }

procedure MsgWaitForSingleObject(Handle: THandle);
begin
  repeat
	 if MsgWaitForMultipleObjects( 1, Handle, False, INFINITE, QS_ALLINPUT)
		 = WAIT_OBJECT_0 + 1
		then Application.ProcessMessages
		else BREAK;
  until True = False;
end;

function SpawnProcess(const Command: string): TProcessInformation;
var StartupInfo: TStartupInfo;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), 0); // use defaults
  StartupInfo.cb := SizeOf(StartupInfo);
  CreateProcess( Nil, PChar(Command), Nil, Nil, False, 0, Nil,
					  Nil, StartupInfo, Result );
end;

{ ============== TWaitThread ============= }

procedure WaitForThread( Handler: TThreadMethod; Data: pointer );
begin
  TWaitThread.CreateWait(Handler, Data).WaitFor;
end;

procedure MsgWaitForThread( var Thread: TWaitThread;
				Handler: TThreadMethod; Data: pointer );
begin
  Thread := TWaitThread.CreateWait(Handler, Data);
  Thread.MsgWaitFor;  //
  Thread := Nil;
end;

constructor TWaitThread.CreateWait( _Action: TThreadMethod; _Data: pointer );
begin
  CreateSimple(_Action, True, _Data); // CreateSuspended
  AbortFlag := Nil;
end;

procedure TWaitThread.WaitFor;
begin
  Run(False);
end;

procedure TWaitThread.MsgWaitFor;
begin
  Run(True);
end;

procedure TWaitThread.Run(MsgWait: boolean);
var
  Aborted: boolean;
begin
  AbortFlag := @ Aborted;
  Aborted   := False;
  Resume;
  if MsgWait
	 then MsgWaitForSingleObject(Handle)
    else inherited WaitFor;
  if Aborted then Abort;
end;

procedure TWaitThread.AbortThread;
begin
  Assert(Assigned(AbortFlag));
  AbortFlag^ := True;
  inherited;
end;


{ ============== TStopStartThread ============= }

constructor TStopStartThread.Create;
begin
  // CreateEvent API call is smaller and simpler than Delphi wrapper
  Event := CreateEvent(Nil, True, False, Nil);
  Assert(Event <> 0);
  Waiting := False;
  Aborted := False;
  inherited Create(True); // Create a suspended thread
end;

destructor TStopStartThread.Destroy;
begin
  CloseHandle(Event);
  inherited;
end;

procedure  TStopStartThread.Execute;
begin
  while not Terminated do
	 if Assigned(ThreadMethod) then begin
		 ThreadMethod(Data);		//Do the work
		 SetEvent(Event);
		 Suspend;
		 end;
end;

procedure TStopStartThread.Run( _Action: TThreadMethod; _Data:pointer;
										  MsgWait: boolean );
begin
  if Waiting then raise EThreadInUse.Create('Thread in use');
  if Aborted then raise EAbortedThread.Create('Aborted thread');

  ThreadMethod := _Action;
  Data         := _Data;
  Waiting      := True;
  ResetEvent(Event);
  Resume;		//Starts Execute again
  if MsgWait
	 then MsgWaitForSingleObject(Event)
	 else WaitForSingleObject	 (Event, INFINITE);
  Waiting := False;
  if Aborted then Abort; // Raise an EAbort exception
end;

procedure TStopStartThread.MsgWaitFor( _Action: TThreadMethod; _Data:pointer );
begin
  Run(_Action, _Data, True);
end;

procedure TStopStartThread.WaitFor( _Action: TThreadMethod; _Data:pointer );
begin
  Run(_Action, _Data, False);
end;

procedure TStopStartThread.AbortThread;
begin
  Suspend; // Can't kill a running thread by Freeing it
  Aborted := True;
  SetEvent(Event);
end;

end.
