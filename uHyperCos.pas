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

unit uHyperCos;

interface

uses
  uFirmata;

type
  // IHyperCosDisplay defines the interface THyperCos
  // expects for interacting with the user.
  // TODO: Update the handling of Firmata connection status to work like Scratch connection.
  IHyperCosDisplay = interface
    procedure ShowFirmataConnectionStatus( const Port: string; const Speed: Integer; const Version: string; const Connected: Boolean );
    procedure ShowScratchConnectionStatus( const Host: string; Port: Integer; const Connected: Boolean );
    procedure ShowErrorText( const Text: string );
    procedure ShowLastCmdText( const Text: string );
    procedure ShowPinState( const Pin: Integer; const State: Boolean );
    procedure ShowPinMode( const Pin: Integer; const Mode: TPinMode );
  end;

  // IHyperCos defines the interface used to control HyperCos.
  IHyperCosControl = interface
    procedure InjectCommand( const Command: string );
  end;

  // HyperCosFactory sets up a HyperCos instance
  // and returns a control interface for it. Use the Project|View Source
  // Delphi menu option to view the contents of the .DPR file to see
  // where this is used.
  THyperCosFactory = class
    public class function GetHyperCos(  Display: IHyperCosDisplay; ScratchHost: string; ScratchPort: Integer=42001 ): IHyperCosControl;
  end;

implementation

uses
  uScratch, SysUtils, Classes, ExtCtrls;

type
  EHyperCos = class( Exception );
  
type
  // HyperCos is the conduit between Scratch and Firmata. THyperCos
  // is the class that implements the plumbing that does the work.
  // Rather than expose THyperCos to the user directly, the THyperCosFactory
  // class sets up an instance of THyperCos and returns a reference to the
  // IHyperCosControl interface which provides just the control functions
  // required to use THyperCos.
  THyperCos = class( TInterfacedObject, IHyperCosControl )
  private
    Display : IHyperCosDisplay;
    Firmata : TDuemilanoveFirmata;
    Scratch : TScratchConnection;

    tmrScratchReconnect : TTimer;
  protected
    // Scratch connection maintenance
    procedure OnTimerScratchReconnect( Sender: TObject );

    // Scratch command decoding and handling
    procedure CommandDecode(const Command: string);
    procedure ArduinoPortCmd( Value: Integer );
    procedure ResetCmd( );
    procedure PinModeCmd( Pin: Byte; Mode: TPinMode );

    procedure DigitalWriteCmd( Pin: Byte; State: Boolean );
    procedure ReportDigitalCmd( Port: Byte; Enable: Boolean );

    procedure AnalogWriteCmd( Pin: Byte; Value: Integer );
    procedure ReportAnalogCmd( Pin: Byte; Enable: Boolean );

    // Event sinks for firmata and scratch
    procedure OnScratchConnectStatus(Sender: TObject );
    procedure OnScratchCommand(Sender: TObject; Command: string );
    procedure OnScratchError(Sender: TObject; ErrorMsg: string);
    procedure OnFirmataDigitalInput(Sender: TObject; Port: Byte );
    procedure OnFirmataAnalogInput(Sender: TObject; Pin: Byte );
    procedure OnFirmataVersion(Sender: TObject );
    procedure OnFirmataError(Sender: TObject; ErrorMsg: string);

    // IHyperCosControl methods
    procedure InjectCommand( const Command: string );

  public
    constructor Create( Display: IHyperCosDisplay; ScratchHost: string; ScratchPort: Integer ); virtual;
    destructor Destroy(); override;
  end;

{ THyperCosFactory }
class function THyperCosFactory.GetHyperCos( Display: IHyperCosDisplay; ScratchHost: string; ScratchPort: Integer=42001): IHyperCosControl;
begin
  result := THyperCos.Create( Display, ScratchHost, ScratchPort );
end;


//=====================================================================
//  Procedure: Create
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Constructor for HyperCos
constructor THyperCos.Create( Display: IHyperCosDisplay; ScratchHost: string; ScratchPort: Integer );
var
  i : Integer;
begin
  inherited Create;

  self.Display := Display;

  // One way to pass necessary information to an object is to
  // provide it as parameters to the object's constructor. This
  // can simplify some aspects of the object setup, but on the
  // other hand, it can make the constructor parameter list rather long.
  Scratch := TScratchConnection.Create( ScratchHost,
                                        ScratchPort,
                                        OnScratchCommand,
                                        OnScratchConnectStatus,
                                        OnScratchError );

  // Timer component used to retry scratch connection
  tmrScratchReconnect := TTimer.Create(nil);
  tmrScratchReconnect.Interval := 500;
  tmrScratchReconnect.OnTimer := OnTimerScratchReconnect;
  tmrScratchReconnect.Enabled := True;

  // Set up the board object without a comm port configured.
  // Scratch will provide one when it receives a port command.
  // Another way to pass necessary information to an object is
  // to provide it after construction by assigning it to properties
  // on the object. This makes the constructor cleaner, but doesn't
  // allow the programmer to raise an exception at object creation
  // if the user forgets to provide a necessary event sink.
  Firmata := TDuemilanoveFirmata.Create();
  Firmata.OnDigitalInput := OnFirmataDigitalInput;
  Firmata.OnAnalogInput := OnFirmataAnalogInput;
  Firmata.OnVersionInput := OnFirmataVersion;
  Firmata.OnError := OnFirmataError;

  // Firmata doesn't have a comm port configuration yet, so use
  // the DelayWrite flag to prevent it attempting to use the port.
  Firmata.DelayWrite := True;

  // Configure the Duemilanove pins for digital output mode.
  // If desired some pins could be used for input so that
  // data could be fed back to Scratch.
  for i := 2 to 13 do
      Firmata.DigitalPin[i].Mode := pmDigitalOutput;
  Firmata.DelayWrite := False;
end;


//=====================================================================
//  Procedure: Destroy
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Destructor for HyperCos
destructor THyperCos.Destroy;
begin
  Scratch.Free;
  Firmata.Free;
  tmrScratchReconnect.Free;

  inherited;
end;


//=====================================================================
//  Procedure: OnScratchCommand
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Process a command received from Scratch
procedure THyperCos.OnScratchCommand(Sender: TObject; Command: string);
begin
  try
    // Display and decode the command
    Display.ShowLastCmdText( Command );
    CommandDecode( Command );
  except
    on E:Exception do
      Display.ShowErrorText( E.Message );
  end;
end;


//=====================================================================
//  Procedure: OnScratchConnectStatus
//  Author:    DavidK
//  Date:      03-Oct-2009
//  Comments:  React to scratch connection changes
procedure THyperCos.OnScratchConnectStatus(Sender: TObject );
begin
  Display.ShowScratchConnectionStatus( Scratch.Host, Scratch.Port, Scratch.Connected );
  tmrScratchReconnect.Enabled := not Scratch.Connected;
end;


//=====================================================================
//  Procedure: OnScratchError
//  Author:    DavidK
//  Date:      05-Oct-2009
//  Comments:  Scratch error message handler. Since the Scratch
//             connection handler has a threaded portion that can
//             generate errors at any time I need a place to catch
//             errors that it can generate. Rather than simply raising
//             them and relying on the Application exception processing
//             to catch them for me, I'm providing an event handler
//             to pass them back to be handled normally.
procedure THyperCos.OnScratchError(Sender: TObject; ErrorMsg: string);
begin
  Display.ShowErrorText( ErrorMsg );
end;


//=====================================================================
//  Procedure: OnFirmataDigitalInput
//  Author:    DavidK
//  Date:      14-Oct-2009
//  Comments:  Firmata has received digital input. Send to Scratch.
procedure THyperCos.OnFirmataDigitalInput(Sender: TObject; Port: Byte);
var
  i : Integer;
  Pins : TPinsArray;
begin
  try
    Pins := Firmata.DigitalPort[Port].ChangedPins;

    for i := 0 to High(Pins) do begin
      // Send an update message and reset the changed flag
      Scratch.SensorUpdate( Pins[i].BoardPin, Integer(Pins[i].State));
      Pins[i].Changed := False;
    end;
  except
    on E:Exception do
      Display.ShowErrorText( E.Message );
  end;
end;


//=====================================================================
//  Procedure: OnFirmataAnalogInput
//  Author:    DavidK
//  Date:      14-Oct-2009
//  Comments:  Firmata has received analog input. Send to Scratch.
procedure THyperCos.OnFirmataAnalogInput(Sender: TObject; Pin: Byte);
begin
  // To avoid confusion in Scratch, analog pins will
  // be referred to as Pin14-Pin19
  Scratch.SensorUpdate(Firmata.AnalogPinsOffset+Pin, Firmata.AnalogPin[Pin].Value );
end;


//=====================================================================
//  Procedure: OnFirmataVersion
//  Author:    DavidK
//  Date:      09-Oct-2009
//  Comments:  Firmata version received
procedure THyperCos.OnFirmataVersion(Sender: TObject);
begin
  Display.ShowFirmataConnectionStatus( Firmata.CommPort, Firmata.CommBaud, Firmata.FirmataVersion, Firmata.CommOpen );
end;


//=====================================================================
//  Procedure: OnFirmataError
//  Author:    DavidK
//  Date:      14-Oct-2009
//  Comments:  
procedure THyperCos.OnFirmataError(Sender: TObject; ErrorMsg: string);
begin
  Display.ShowErrorText( ErrorMsg );
end;


//=====================================================================
//  Procedure: ThreadScratchConnect
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Threaded method that retries Scratch connection.
procedure THyperCos.OnTimerScratchReconnect(Sender: TObject);
begin
  try
    Scratch.OpenConnection;
  except
    on E:Exception do
      Display.ShowErrorText(E.Message);
  end;
end;


//=====================================================================
//  Procedure: CommandDecode
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Simple command decoder for Scratch commands.
//             Just follow the pattern to add more.
//             This architecture is intended to be simple and
//             easy to understand. More sophisticated designs
//             that provide a clear separation between code
//             and data are possible and preferable, from a
//             maintenance and extensibility perspective.
procedure THyperCos.CommandDecode(const Command: string);
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  try
    // split the command at the spaces
    sl.Delimiter := ' ';
    sl.CommaText := lowercase(Command);

    // Process each possible command

    // The arduinoport command is used to set the comm port
    // where the arduino board is attached. It has one parameter,
    // the comm port number.
    if sl[0] = 'arduinoport' then begin
      ArduinoPortCmd( StrToInt(sl[1]) );
    end;

    // The reset command is used to reset the arduino board.
    if sl[0] = 'reset' then begin
      ResetCmd( );
    end;

    // The pinmode command is used to set the mode of an arduino pin.
    // It has two parameters, the pin number and the mode.
    // Ex: ^pinmode 13 DigitalOut
    if sl[0] = 'pinmode' then begin
      PinModeCmd( StrToInt(sl[1]), TFirmata.StrToPinMode(sl[2]));
    end;

    // The digitalwrite command is used to set the state of an arduino pin.
    // It has two parameters, the pin number and the state.
    if sl[0] = 'digitalwrite' then begin
      DigitalWriteCmd( StrToInt(sl[1]), sl[2]='high' );
    end;

    // The reportdigital command is used to enable or disable automatic
    // reporting of changes to digital input pins.
    // It has two parameters, the port number and whether sampling is
    // enabled or disabled.
    // Ex: ^ReportDigital 0 Enable
    if sl[0] = 'reportdigital' then begin
      ReportDigitalCmd( StrToInt(sl[1]), sl[2]='enable' );
    end;

    // The analogwrite command is used to set the analog output level of
    // an arduino pin.
    // It has two parameters, the pin number and the value.
    if sl[0] = 'analogwrite' then begin
      AnalogWriteCmd( StrToInt(sl[1]), StrToInt(sl[2]));
    end;

    // The reportanalog command is used to enable or disable automatic
    // reporting of analog samples from an input pin.
    // It has two parameters, the pin number and whether sampling is
    // enabled or disabled.
    if sl[0] = 'reportanalog' then begin
      ReportAnalogCmd( StrToInt(sl[1]), sl[2]='enable' );
    end;

  finally
    sl.Free;
  end;
end;


//=====================================================================
//  Procedure: ReportAnalogCmd
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Pass 'Analog Enable' command from scratch to Firmata
procedure THyperCos.ReportAnalogCmd(Pin: Byte; Enable: Boolean);
begin
  try
    Firmata.AnalogPin[Pin].Reporting := Enable;
  except
    on E:Exception do
      Display.ShowErrorText( E.Message );
  end;
end;


//=====================================================================
//  Procedure: AnalogWriteCmd
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Pass command from scratch to Firmata
procedure THyperCos.AnalogWriteCmd( Pin: Byte; Value: Integer);
begin
  try
    Firmata.DigitalPin[Pin].DutyCycle := Value;
  except
    on E:Exception do
      Display.ShowErrorText( E.Message );
  end;
end;


//=====================================================================
//  Procedure: ArduinoPortCmd
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Pass command from scratch to Firmata
procedure THyperCos.ArduinoPortCmd(Value: Integer);
begin
  try
    Firmata.CommPort := 'COM'+IntToStr(Value);
    Display.ShowFirmataConnectionStatus( Firmata.CommPort, Firmata.CommBaud, Firmata.FirmataVersion, Firmata.CommOpen );
  except
    on E:Exception do
      Display.ShowErrorText( E.Message );
  end;
end;


//=====================================================================
//  Procedure: DigitalWriteCmd
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Pass command from scratch to Firmata
procedure THyperCos.DigitalWriteCmd(Pin: Byte; State: Boolean);
begin
  try
    // Set the Firmate pin
    Firmata.DigitalPin[Pin].State := State;
    // Update the display to show the current state
    Display.ShowPinState( Pin, State );
  except
    on E:Exception do
      Display.ShowErrorText( E.Message );
  end;
end;


//=====================================================================
//  Procedure: ReportDigitalCmd
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Pass 'Report Digital' command from scratch to Firmata
procedure THyperCos.ReportDigitalCmd(Port: Byte; Enable: Boolean);
begin
  try
    Firmata.DigitalPort[Port].Reporting := Enable;
  except
    on E:Exception do
      Display.ShowErrorText( E.Message );
  end;
end;


//=====================================================================
//  Procedure: PinModeCmd
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Pass command from scratch to Firmata
procedure THyperCos.PinModeCmd(Pin: Byte; Mode: TPinMode);
begin
  try
    // Scratch refers to Duemilanove analog pins as pins 14..19.
    if Pin >= Firmata.AnalogPinsOffset then begin
      Firmata.AnalogPin[Pin-Firmata.AnalogPinsOffset].Mode := Mode;
      Display.ShowPinMode( Pin-Firmata.AnalogPinsOffset, Mode );
    end else begin
      Firmata.DigitalPin[Pin].Mode := Mode;
      Display.ShowPinMode( Pin, Mode );
    end;

  except
    on E:Exception do
      Display.ShowErrorText( E.Message );
  end;
end;


//=====================================================================
//  Procedure: ResetCmd
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Pass command from scratch to Firmata
procedure THyperCos.ResetCmd();
begin
  try
    Firmata.Reset;
  except
    on E:Exception do
      Display.ShowErrorText( E.Message );
  end;
end;


//=====================================================================
//  Procedure: InjectCommand
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Inject a command via the control interface
procedure THyperCos.InjectCommand(const Command: string);
begin
  CommandDecode( Command );  
end;


end.
