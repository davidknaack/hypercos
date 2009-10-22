//=============================================================================
//    HyperCos v1 Arduino to Scratch bridge
//    Copyright (C) 2009 David Knaack (davidknaack@gmail.com)
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
//=============================================================================

unit uScratch;

interface

uses
  Contnrs, SynchedThreads, IdTCPClient, SysUtils, Classes;

type
  TScratchConnection = class;

  TNewCommandEvent = procedure(Sender: TObject; Command: string ) of object;
  TErrorEvent = procedure(Sender: TObject; Error: string ) of object;

  TScratchConnection = class
  private
    Err : string;
    tcpConnection : TIdTCPClient;
    cmdQueue : TObjectQueue;
    ReadThread : TSimpleThread;

    fOnConnectStatusChanged : TNotifyEvent;
    fOnNewCommand : TNewCommandEvent;
    fOnError : TErrorEvent;
  protected
    function GetConnected: Boolean; virtual;
    function GetHost: string;
    function GetPort: integer;
    procedure DoConnectStatusChanged( );
    procedure DoErrorEvent( ErrorMsg: string );

    procedure SocketReadThread(data: pointer); virtual;
    procedure ReadThreadDone( sender: TObject ); virtual;

    procedure ProcessNewCommand; virtual;
    procedure HandleException; virtual;
  public
    procedure OpenConnection; virtual;
    procedure CloseConnection; virtual;

    procedure SensorUpdate( Pin, Value: Integer ); virtual;

    constructor Create( Host: string; Port: Word; OnNewCommand: TNewCommandEvent; OnConnectChanged: TNotifyEvent; OnError: TErrorEvent );
    destructor Destroy; override;

    property Host: string read GetHost;
    property Port: integer read GetPort;
    property Connected: Boolean read GetConnected;
    property OnConnectStatusChanged: TNotifyEvent read fOnConnectStatusChanged write fOnConnectStatusChanged;
    property OnNewCommand: TNewCommandEvent read fOnNewCommand write fOnNewCommand;
    property OnError: TErrorEvent read fOnError write fOnError;
  end;

implementation

uses
  Forms, IdException, IdExceptionCore, IdStack;

type
   //=====================================
   // A simple boxed string class. Allows
   // the user to contain a string within
   // and object. This is useful for storing
   // strings where an object is expected.
   TString = class(TObject)
     Val: String;
     constructor Create(const Val: String) ;
   end;

constructor TString.Create(const Val: String) ;
begin
   inherited Create;
   self.Val := Val;
end;


{ TScratchConnection }

//=====================================================================
//  Procedure: Create
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:
constructor TScratchConnection.Create( Host: string; Port: Word;
                                       OnNewCommand: TNewCommandEvent;
                                       OnConnectChanged: TNotifyEvent;
                                       OnError: TErrorEvent );
begin
  cmdQueue := TObjectQueue.Create;

  tcpConnection := TIdTCPClient.Create(nil);
  tcpConnection.Host := Host;
  tcpConnection.Port := Port;

  fOnNewCommand := OnNewCommand;
  fOnConnectStatusChanged := OnConnectChanged;
  fOnError := OnError;
end;


//=====================================================================
//  Procedure: Destroy
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:
destructor TScratchConnection.Destroy;
begin
  CloseConnection;
  cmdQueue.Free;
  tcpConnection.Free;
end;


//=====================================================================
//  Procedure: GetConnected
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Return whether a connection to Scratch has been opened.
//             Use the existence of the ReadThread instance as an indicator.
function TScratchConnection.GetConnected: Boolean;
begin
  result := False;
  try
    result := tcpConnection.Connected; //Assigned( ReadThread );
  except
    // Assume that any error coming out of tcpConnection means
    // that it isn't connected. If the other end has dropped
    // the connection it is possible to get an exception when
    // reading .Connected.
  end;
end;


//=====================================================================
//  Procedure: GetHost
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Return scratch host
function TScratchConnection.GetHost: string;
begin
  result := tcpConnection.Host;
end;


//=====================================================================
//  Procedure: GetPort
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Return scratch port
function TScratchConnection.GetPort: integer;
begin
  result := tcpConnection.Port;
end;


//=====================================================================
//  Procedure: DoConnectStatusChanged
//  Author:    DavidK
//  Date:      03-Oct-2009
//  Comments:  Call OnConnectStatusChanged, if assigned
procedure TScratchConnection.DoConnectStatusChanged;
begin
  if Assigned( fOnConnectStatusChanged ) then
    fOnConnectStatusChanged( self );
end;


//=====================================================================
//  Procedure: DoErrorEvent
//  Author:    DavidK
//  Date:      05-Oct-2009
//  Comments:  Call OnErrorEvent, if assigned
procedure TScratchConnection.DoErrorEvent( ErrorMsg: string );
begin
  if Assigned( fOnError ) then
    fOnError( self, ErrorMsg );
end;


//=====================================================================
//  Procedure: SocketReadThread
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  This routine is executed in a thread and waits for
//             input data from Scratch. It does a bit of processing
//             to pull out the commands intended for HyperCos and
//             then passes the command string back to the application.
procedure TScratchConnection.SocketReadThread( data: pointer );
var
  cmd : string;
  finished : boolean;

begin
  finished := false;

  // Loop forever. The disconnect exception produced by
  // tcpConnection will be used to exit the loop.
  while not finished do begin
    try
      // Read a single command from Scratch
      cmd := tcpConnection.IOHandler.ReadLn('"');

      // Commands for HyperCos are prefixed with a carat (^)
      // so ignore commands that start with anything else.
      if cmd[1] <> '^' then
        Continue;

      // Encapsulate the command into a TString and
      // push it into the command queue. The command queue
      // will be processed in the main thread. The Copy()
      // command returns everything except the carat, which
      // is no longer needed.
      cmdQueue.Push( TString.Create(Copy(cmd,2,Length(cmd))));

      // Call back to the main thread to process the command queue.
      TThread.Synchronize( ReadThread, ProcessNewCommand );
    except
      // EIdClosedSocket indicates a normal shutdown from this
      // end, exit without generating an error message.
      on EIdClosedSocket do
        finished := true;

      on E:EIdException do begin
        if (E is EIdSocketError)
        and (EIdSocketError(E).LastError = 10054) then
          Err := 'Scratch closed the remote sensing connection.'
        else
          Err := E.Message;

        // Always break on Indy problems.
        finished := true;
      end;

      // Non-socket errors are reported and execution continues.
      on E:Exception do
        Err  := E.Message;
    end; // try

    // if an error message was generated, notify the main thread
    if Err <> '' then
      TThread.Synchronize( ReadThread, HandleException );

  end; // while

  // Always try to shut down the tcpConnection before leaving the thread.
  try
    tcpConnection.Disconnect;
  except
    // If it is already closed because of a connection error
    // calling Disconnect can raise an exception.
  end;
end;


//=====================================================================
//  Procedure: HandleException
//  Author:    DavidK
//  Date:      03-Oct-2009
//  Comments:  It's possible to pass the exception object back to the
//             main thread for handling, but it can sometimes get a
//             little complicated. Since I don't need anything but the
//             error message, I'll just send that back.
procedure TScratchConnection.HandleException;
begin
  DoErrorEvent( Err );
  Err := '';
end;


//=====================================================================
//  Procedure: ReadThreadDone
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:
procedure TScratchConnection.ReadThreadDone(sender: TObject);
begin
  DoConnectStatusChanged;
end;


//=====================================================================
//  Procedure: OpenConnection
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Initiates a connection to Scratch and sets up the thread
procedure TScratchConnection.OpenConnection;
begin
  // Open a tcp connection to Scratch
  try
    tcpConnection.Connect;
    DoConnectStatusChanged;
    // Create a thread to read the data coming in from Scratch.
    // When the connection to Scratch is closed
    // FreeOnTerminate is disabled so that on shutdown the
    // WaitFor method can be used. This will allow shutdown
    // without an exception from the tcp components.
    ReadThread := RunInThread( SocketReadThread, False, False, ReadThreadDone );
  except
    on E:EIdSocketError do begin
      if EIdSocketError(e).LastError = 10061 then
        E.Message := 'Is Scratch running with Remote Sensing enabled?';

      raise;
    end;
  end;
end;


//=====================================================================
//  Procedure: CloseConnection
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Shut down the tcp connection to Scratch and kill the thread.
procedure TScratchConnection.CloseConnection;
begin
  // Disconnect from scratch. The TSimpleThread created
  // by RunInThread when the connection was opened will
  // detect that the connection was closed and exit on it's own.
  tcpConnection.Disconnect;
  if Assigned( ReadThread ) then begin
    ReadThread.WaitFor;
    FreeAndNil( ReadThread );
  end;
end;


//=====================================================================
//  Procedure: ProcessNewCommand
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Process the input queue. Normally it will only contain
//             one command, but if necessary multiple commands can be
//             pushed into the queue.
procedure TScratchConnection.ProcessNewCommand;
var
  cmd : TString;
begin
  // Process the entire queue
  while cmdQueue.Count > 0 do begin

    // Check the type of each object found in the queue.
    // Anything that isn't a TString is discarded.
    if not (cmdQueue.Peek is TString) then begin
      cmdQueue.Pop;
      Continue;
    end;

    // Get the command
    cmd := cmdQueue.Pop as TString;

    // Send the command out for processing
    if Assigned( fOnNewCommand ) then
      fOnNewCommand( self, cmd.Val );

    // Dispose of the string object
    cmd.Free;
  end;
end;


//=====================================================================
//  Procedure: SensorUpdate
//  Author:    DavidK
//  Date:      03-Oct-2009
//  Comments:  Send a sensor update message to Scratch
procedure TScratchConnection.SensorUpdate( Pin, Value: Integer );
var
  Msg : string;
begin
  // This is the message format expected by Scratch. The first four
  // bytes are the message length.
  // See http://scratch.mit.edu/forums/viewtopic.php?id=9458 for details.
  Msg := Format( 'XXXXsensor-update "Pin%d" %d', [Pin, Value] );
  Msg[1] := #0;
  Msg[2] := #0;
  Msg[3] := #0;
  Msg[4] := Char(Length( Msg )-4);

  tcpConnection.IOHandler.Write( Msg );
end;

end.
