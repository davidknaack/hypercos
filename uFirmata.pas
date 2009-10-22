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

unit uFirmata;

interface

uses
  CPort, CPortTypes, SynchedThreads, Classes, SysUtils, ExtCtrls;

// Message command bytes
const DIGITAL_MESSAGE      = $90; // send data for a digital pin
const ANALOG_MESSAGE       = $E0; // send data for an analog pin (or PWM)

const PULSE_MESSAGE        = $A0; // proposed pulseIn/Out message (SysEx)
const SHIFTOUT_MESSAGE     = $B0; // proposed shiftOut message (SysEx)

const REPORT_ANALOG_PIN    = $C0; // enable analog input by pin
const REPORT_DIGITAL_PORTS = $D0; // enable digital input by port pair
const START_SYSEX          = $F0; // start a MIDI SysEx message
const SET_PIN_MODE         = $F4; // set a pin mode
const END_SYSEX            = $F7; // end a MIDI SysEx message
const REPORT_VERSION       = $F9; // report firmware version
const SYSTEM_RESET         = $FF; // reset from MIDI

const PWM_PINS : set of Byte = [9, 10, 11];

// Note that the ordinal values of the enumerated values are
// important. They must map to the values expected by Firmata
// for the corresponding constants.
// Pin modes  0: INPUT  1: OUTPUT  2: ANALOG  3: PWM  4:SERVO
type TPinMode = ( pmDigitalInput,
                  pmDigitalOutput,
                  pmAnalogInput,
                  pmDigitalPWM,
                  pmServo,
                  pmUnavailable );
                  
const cPinModes : array[pmDigitalInput..pmUnavailable] of string =
                                                    ('DigitalIn', 'DigitalOut',
                                                     'AnalogIn', 'DigitalPWM',
                                                     'Servo', 'Unavailable' );

type
  TDynByteArray = Array of byte;

type
  EFirmata = class(Exception);
  EPort = class(EFirmata);
  EDigitalPin = class(EFirmata);

type
  TFirmataMessage = class
  private
    buffer : TDynByteArray;
  public
    constructor ReportAnalogPin( Pin : Byte; Enable : Boolean );
    constructor ReportDigitalPort( Port : Byte; Enable : Boolean );
    constructor DigitalIOMessage( Port, Value: Byte );
    constructor AnalogIOMessage( Pin, Value: Byte );
    constructor SetPinMode( Pin: Byte; Mode: TPinMode );
    constructor ReportProtocolVersion;
  end;

  // Todo: see about switching this to Synaser instead of cportu.
  TFirmataComm = class(TComPort)
  public
    procedure Write(const Buffer: TDynByteArray); overload; virtual;
    procedure Write(const Msg: TFirmataMessage ); overload; virtual;
    function Read( Count: Integer ):AnsiString; virtual;
    constructor Create(Port: string; Baud: string); reintroduce; virtual;
  end;

  // Forward declaration for TPort and TFirmata classes
  TPort = class;
  TFirmata = class;

  // All Arduino pins are digital pins.
  TDigitalPin = class
  private
    fPortPin   : Byte;
    fPort      : TPort;
    fMode      : TPinMode;
    fState     : Boolean;
    fChanged   : Boolean;
    fBoardPin  : Byte;
    fDutyCycle : Byte;
  protected
    procedure SetMode(const Mode: TPinMode); virtual;
    procedure SetState(const State: Boolean); virtual;
    function GetState: Boolean;
    procedure SetDutyCycle(const Value: Byte); virtual;
  public
    procedure ModeUpdate;
    procedure DutyCycleUpdate;
    constructor Create( PortPin: Byte; Port: TPort ); virtual;

    property Port: TPort read fPort;
    property PortPin: Byte read fPortPin;
    property Mode: TPinMode read fMode write SetMode;
    property State: Boolean read GetState write SetState;
    property Changed: Boolean read fChanged write fChanged;
    property DutyCycle: Byte read fDutyCycle write SetDutyCycle;
    property BoardPin: Byte read fBoardPin;
  end;

  // Analog pin on the arduino board.
  TAnalogPin = class( TDigitalPin )
  private
    fReporting : Boolean;
    fValue     : Integer;
    fAnalogPin : Byte;
    procedure SetReporting(const Value: Boolean);
    function GetValue: Integer;
  protected
    procedure SetMode(const Mode: TPinMode); override;
  public
    procedure UpdateReporting;
    constructor Create( PortPin: Integer; Port: TPort; AnalogPin: Byte ); reintroduce; virtual;
    property AnalogPin: Byte read fAnalogPin;
    property Reporting: Boolean read fReporting write SetReporting;
    property Value: Integer read GetValue write fValue;
  end;


  TPinsArray = array of TDigitalPin;

  TPort = class
  private
    fFirmata : TFirmata;
    fPort : Byte;
    fReporting : Boolean;
    fValue : Byte;
    fPins : array[0..7] of TDigitalPin;

    delayWrite : Boolean; // Local flag for controlling hardware update
  protected
    procedure SetValue(const Value: Byte);

    procedure SetReporting(const Value: Boolean); virtual;
    function GetPin(Index:Byte): TDigitalPin; virtual;
    function GetChangedPins: TPinsArray; virtual;

    procedure WriteValue;

  public
    procedure ReportingUpdate;
    procedure ModeUpdate;
    procedure DutyCycleUpdate;

    constructor Create( Port: Integer; Firmata: TFirmata); virtual;

    // Firmata instance that owns this port
    property Firmata : TFirmata read fFirmata;

    // Port number, 0 or 1.
    property Port : Byte read fPort;

    // Just the pins that are marked as changed
    property ChangedPins: TPinsArray read GetChangedPins;

    // Indicates whether the hardware should report the values from this port
    property Reporting : Boolean read fReporting write SetReporting;

    // Pin array, access to the individual port pins
    property Pin[Index:Byte]: TDigitalPin read GetPin;

    // Value of all of the pins on this port
    property Value : Byte read fValue write SetValue;
  end;

  // The 'Value' parameter here is a generic value, not the value of
  // a port or an analog reading. What it contains may vary by the
  // type of input event. A digital input event will fill value with
  // the port number, an analog input event will provide the pin number.
  TInputEvent = procedure(Sender: TObject; Value: Byte ) of object;
  TErrorEvent = procedure(Sender: TObject; Error: string ) of object;

  // Base class for Firmata
  TFirmata = class
  private
    fCommPort : TFirmataComm;
    fFirmataVersion : string;
    fDigitalPorts : array of TPort;
    fAnalogPinsOffset : Integer;
    fOnDigitalInput: TInputEvent;
    fOnAnalogInput: TInputEvent;
    fOnVersionInput: TNotifyEvent;
    fOnError : TErrorEvent;
    fPollCommPort : Boolean;
    CheckTimerTimer : TTimer;
    fDelayWrite: Integer;
    procedure SetDelayWrite(const Value: Boolean);
    function GetDelayWrite: Boolean;
  protected
    procedure SetCommPort(const Value: string); virtual;
    function GetCommPort: string; virtual;
    function GetCommBaud: integer; virtual;

    function GetDigitalPort(Index:Byte): TPort; virtual;
    function GetDigitalPin(Index:Byte): TDigitalPin; virtual;
    function GetAnalogPin(Index:Byte): TAnalogPin; virtual;
    procedure SetAnalogPin(Index:Byte; Pin: TAnalogPin ); virtual;

    procedure ProcessDigitalMessage( Port: Byte ); virtual;
    procedure ProcessAnalogMessage( AnalogPin: Byte ); virtual;
    procedure ProcessReportVersion(); virtual;

    procedure InitCommPort;
    function CommAvailable: Boolean;
    procedure CheckTimer( Sender: TObject );
    procedure CheckInput; virtual;
    procedure DoErrorEvent( ErrorMsg: string );
  public
    class function StrToPinMode( const Mode: string ): TPinMode;
    class function PinModeToStr( const Mode: TPinMode ): string;
    constructor Create(DigitalPinCount, AnalogPinsCount, AnalogPinsOffset: Integer; CommPort: TFirmataComm=nil); virtual;
    destructor Destroy; override;
    procedure Reset; virtual;
    procedure Write( FirmataMsg: TFirmataMessage ); virtual;

    property CommPort: string read GetCommPort write SetCommPort;
    property CommBaud: integer read GetCommBaud;
    property PollCommPort: Boolean read fPollCommPort write fPollCommPort;
    property FirmataVersion: string read fFirmataVersion;
    property DigitalPort[Index:byte]: TPort read GetDigitalPort;
    property DigitalPin[Index:Byte]: TDigitalPin read GetDigitalPin;
    property AnalogPin[Index:byte]: TAnalogPin read GetAnalogPin;
    property AnalogPinsOffset: Integer read fAnalogPinsOffset;
    property CommOpen: Boolean read CommAvailable;
    property DelayWrite: Boolean read GetDelayWrite write SetDelayWrite;

    property OnAnalogInput: TInputEvent read FOnAnalogInput write fOnAnalogInput;
    property OnDigitalInput: TInputEvent read FOnDigitalInput write fOnDigitalInput;
    property OnVersionInput: TNotifyEvent read FOnVersionInput write fOnVersionInput;
    property OnError: TErrorEvent read fOnError write fOnError;
  end;

  // Firmata interface set up for a Duemilanove-compatable board.
  // 20 Digital pins addressed as 0..19
  //  6 Analog pins addressed as 0..5 and mapped onto digital pins 14..19
  // Pins 0 and 1 not available.
  TDuemilanoveFirmata = class( TFirmata )
  private
    procedure DuemilanoveSetup;
  public
    constructor Create(CommPort: string; Baud: string = '57600'); reintroduce; overload; virtual;
    constructor Create(); reintroduce; overload; virtual;
  end;

implementation

uses
  Windows;

{ TFirmataComm }


//=====================================================================
//  Procedure: Create
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Create a standard comm port, but init with settings
//             suitable for Firmata.
constructor TFirmataComm.Create(Port: string; Baud: string);
begin
  inherited Create( nil );

  BaudRate := StrToBaudRate(Baud);
  Self.Port := Port;
end;


//=====================================================================
//  Procedure: Write
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Sends data provided as a dynamic array of bytes
procedure TFirmataComm.Write(const Buffer: TDynByteArray);
begin
  Write( PAnsiChar(@Buffer[0]), Length(Buffer));
end;


//=====================================================================
//  Procedure: Write
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Sends data provided as a TFirmataMessage instance
//             For simplicity the TFirmataMessage instance is
//             freed here; the caller should not try to reuse it.
procedure TFirmataComm.Write(const Msg: TFirmataMessage);
begin
  Write( msg.buffer );
  msg.Free;
end;


//=====================================================================
//  Procedure: Read
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Attempt to read the specified number of bytes from the
//             comm port. The caller is responsible for checking whether
//             the required data was returned.
function TFirmataComm.Read( Count: Integer ): AnsiString;
begin
  SetLength( result, Count );
  SetLength( result, ReadStr( result, Count ));
end;



{ TFirmataMessage }

 // =========================== Firmata Protocol V2.1 ==========================
 //                        http://firmata.org/wiki/Protocol
 // Type                Command   MIDI      first byte            second byte
 //                              channel
 //----------------------------------------------------------------------------
 // analog I/O message    0xE0   pin #      LSB(bits 0-6)         MSB(bits 7-13)
 // digital I/O message   0x90   port       LSB(bits 0-6)         MSB(bits 7-13)
 // report analog pin     0xC0   pin #      disable/enable(0/1)   - n/a -
 // report digital port   0xD0   port       disable/enable(0/1)   - n/a -
 //
 // sysex start           0xF0
 // set pin mode(I/O)     0xF4              pin # (0-127)         pin state(0=in)
 // sysex end             0xF7
 // protocol version      0xF9              major version         minor version
 // system reset          0xFF


//=====================================================================
//  Procedure: AnalogIOMessage
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Fill a buffer with the necessary parameters for the
//             'analog I/O message' type. This message type writes
//             analog values to digital pins to set up digital PWM.
//             Pins 9, 10 and 11 are supported on the Arduino.
constructor TFirmataMessage.AnalogIOMessage(Pin, Value: Byte);
begin
  inherited Create;

  SetLength( buffer, 3 );
  Buffer[0] := ANALOG_MESSAGE + Pin;
  Buffer[1] := Value and $7F;
  Buffer[2] := Value shr 7;
end;


//=====================================================================
//  Procedure: DigitalIOMessage
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Fill a buffer with the necessary parameters for the
//             'digital I/O message' type. This message type writes
//             the value of digital pins.
constructor TFirmataMessage.DigitalIOMessage(Port, Value: Byte);
begin
  inherited Create;

  SetLength( buffer, 3 );
  Buffer[0] := DIGITAL_MESSAGE + Port;
  Buffer[1] := Value and $7F;
  Buffer[2] := Value shr 7;
end;


//=====================================================================
//  Procedure: ReportAnalogPin
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Fill a buffer with the necessary parameters for the
//             'report analog pin' message type. This message type
//             directs the hardware to send analog values read from
//             the specified pin.
constructor TFirmataMessage.ReportAnalogPin( Pin : Byte; Enable : Boolean );
begin
  inherited Create;

  SetLength( buffer, 2 );
  Buffer[0] := REPORT_ANALOG_PIN + Pin;
  Buffer[1] := Byte(Enable) and $7F;
end;


//=====================================================================
//  Procedure: ReportDigitalPort
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Fill a buffer with the necessary parameters for the
//             'report digital port' message type. This message type
//             directs the hardware to send the value read from the
//             specified port.
constructor TFirmataMessage.ReportDigitalPort(Port: Byte; Enable: Boolean);
begin
  inherited Create;

  SetLength( buffer, 2 );
  Buffer[0] := REPORT_DIGITAL_PORTS + port;
  Buffer[1] := Byte(Enable) and $7F;
end;


//=====================================================================
//  Procedure: SetPinMode
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Fill a buffer with the necessary parameters for the
//             'set pin mode(I/O)' message type. This message type
//             directs the hardware to set up a pin for the various
//             input and output modes.
constructor TFirmataMessage.SetPinMode(Pin: Byte; Mode: TPinMode);
begin
  inherited Create;

  SetLength( buffer, 3 );
  Buffer[0] := SET_PIN_MODE;
  Buffer[1] := Pin and $7F;
  Buffer[2] := Byte(Mode);
end;


//=====================================================================
//  Procedure: ReportProtocolVersion
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Fill a buffer with the necessary parameters for the
//             'protocol version' message type.
constructor TFirmataMessage.ReportProtocolVersion;
begin
  inherited create;

  SetLength( buffer, 1 );
  Buffer[0] := REPORT_VERSION;
end;



{ TFirmata }

//=====================================================================
//  Procedure: StrToPinMode
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Map a string to a pin mode
class function TFirmata.StrToPinMode(const Mode: string): TPinMode;
var
  i : TPinMode;
begin
  // Iterate through the pin modes
  for i := Low( TPinMode ) to High( TPinMode ) do
    // Compare the text mapping of each pin mode to the input string
    if SameText( Mode, cPinModes[i] ) then begin
      // When a match is found, return it and exit
      result := i;
      Exit;
    end;

  // No match found, generate an error
  raise EFirmata.CreateFmt( 'Specified string "%s" does not '+
                            'conform to a known pin mode.', [Mode] );
end;


//=====================================================================
//  Procedure: PinModeToStr
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Map a pin mode to a string
class function TFirmata.PinModeToStr(const Mode: TPinMode): string;
begin
  if (Mode < Low(TPinMode)) or (Mode > High(TPinMode)) then
    raise EFirmata.CreateFmt( 'Specified PinMode "%d" is not in the'+
                              'valid range of PinModes.', [Integer(Mode)]);
  result := cPinModes[Mode];
end;


//=====================================================================
//  Procedure: Create
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:
constructor TFirmata.Create(DigitalPinCount, AnalogPinsCount, AnalogPinsOffset: Integer; CommPort: TFirmataComm);
var
  i : Integer;
  PortCount : Integer;
begin
  fCommPort := CommPort;

  if (AnalogPinsCount+AnalogPinsOffset) > (DigitalPinCount) then
    raise Exception.Create( 'Invalid configuration, not enough digital pins to support requested analog pins' );

  fAnalogPinsOffset := AnalogPinsOffset;

  PortCount := DigitalPinCount div 8;
  if (DigitalPinCount mod 8) > 0 then
    Inc( PortCount );

  // Set up digital ports.
  SetLength( fDigitalPorts, PortCount );
  for i := 0 to PortCount-1 do
    fDigitalPorts[i] := TPort.Create( i, self );

  // Set up analog pins. This is done by replacing some
  // of the digital pins with analog pins.
  for i := 0 to AnalogPinsCount-1 do
    SetAnalogPin( i+AnalogPinsOffset, TAnalogPin.Create( i+AnalogPinsOffset, fDigitalPorts[(i+AnalogPinsOffset) div 8], i ));

  // Timer object which will check for port input
  CheckTimerTimer := TTimer.Create( nil );
  CheckTimerTimer.OnTimer := CheckTimer;
  CheckTimerTimer.Interval := 10; // 10mS interval
  CheckTimerTimer.Enabled := True;

  // If a comm port was provided, open it and get started.
  if Assigned( fCommPort ) then
    InitCommPort;
end;


//=====================================================================
//  Procedure: Destroy
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:
destructor TFirmata.Destroy();
begin
  fCommPort.Free;
  inherited;
end;


//=====================================================================
//  Procedure: ProcessDigitalMessage
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  A 'digital I/O message' command message for the
//             specifed port was received. Process the next bytes
//             associated with this command.
procedure TFirmata.ProcessDigitalMessage( Port: Byte );
var
  Value : AnsiString;
begin
  Value := fCommPort.Read(2);

  if Length(Value) < 2 then
    raise EFirmata.Create( 'ProcessDigitalMessage unable to read command value bytes' );

  // Use the DelayWrite indicator to block writing
  // of the port value to the hardware. That would
  // normally happen as a result of setting the port
  // value. However, here in ProcessDigitalMessage
  // we know that the hardware triggered the value
  // change, so there is no need to send an update.
  // Note that there is also a TPort.delayWrite that
  // performs a similar function at the port level.
  DelayWrite := True;
  try
    fDigitalPorts[port].Value := (Byte(value[1])) or (Byte(value[2]) shl 7 );
  finally
    // DelayWrite is turned off again in the
    // finally part of this try..finally block
    // to prevent any errors that might happen
    // when the value is set from leaving the
    // DelayWrite flag on. That would prevent
    // any more updates going out to the hardware.
    DelayWrite := False;
  end;

  // Notify any listeners that digital input has been
  // received from the hardware.
  if Assigned( fOnDigitalInput ) then
    fOnDigitalInput( self, port );
end;


//=====================================================================
//  Procedure: ProcessAnalogMessage
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  An 'analog I/O message' command message for the
//             specifed pin was received. Process the next bytes
//             associated with this command.
procedure TFirmata.ProcessAnalogMessage( AnalogPin: Byte );
var
  Value : AnsiString;
begin
  Value := fCommPort.Read(2);

  if Length(Value) < 2 then
    raise Exception.Create( 'ProcessAnalogMessage unable to read command value bytes' );

  self.AnalogPin[AnalogPin].Value := ((Byte(value[1])) or Byte(value[2])shl 7);
  if Assigned( fOnAnalogInput ) then
    fOnAnalogInput( self, AnalogPin );
end;


//=====================================================================
//  Procedure: ProcessReportVersion
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  A 'protocol version' sysex message was received. Process
//             the next bytes associated with this command.
procedure TFirmata.ProcessReportVersion;
var
  Value : AnsiString;
begin
  Value := fCommPort.Read(2);

  if Length(Value) < 2 then
    raise Exception.Create( 'ProcessReportVersion unable to read command value bytes' );

  fFirmataVersion := Format( 'ver%d.%d', [ord(Value[1]), ord(Value[2])]);
  if Assigned( fOnVersionInput ) then
    fOnVersionInput( self );
end;


//=====================================================================
//  Procedure: InitCommPort
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:
procedure TFirmata.InitCommPort;
begin
  fCommPort.Open;
  sleep(2000); // Allow 2 secs for Diecimila auto-reset

  // Request firmata version
  fCommPort.Write( TFirmataMessage.ReportProtocolVersion );

  // The CheckTimer event will check this before it accesses the port.
  PollCommPort := True;
end;


//=====================================================================
//  Procedure: CommAvailable
//  Author:    DavidK
//  Date:      28-Sep-2009
//  Comments:  Return whether the port is accessable.
function TFirmata.CommAvailable:Boolean;
begin
  // Note that short-circuit boolean eval is required here.
  result := Assigned( fCommPort ) and fCommPort.Connected;
end;


//=====================================================================
//  Procedure: CheckTimer
//  Author:    DavidK
//  Date:      21-Aug-2009
//  Comments:  Timer event for reading input from the comm port.
procedure TFirmata.CheckTimer(Sender:TObject);
begin
  try
    if PollCommPort then
      CheckInput;
  except
    on e: Exception do begin
      DoErrorEvent( e.message );
    end;
  end;
end;


//=====================================================================
//  Procedure: CheckInput
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Check the serial port for input. If any is found, decode
//             it and call the appropriate handler.
//             ToDo: This is a very minimal Firmata input parser. It does
//             not correctly handle SysEx messages and should be reworked
//             to improve the SysEx start/stop handling.
procedure TFirmata.CheckInput;
var
  sdata : string;
  data  : Byte;
begin
  if not CommAvailable then
    Exit;

  // Read a byte from Arduino's serial port
  sdata := fCommPort.Read(1);

  if length(sdata) = 0 then
    Exit;

  data := byte(sdata[1]);

  // The upper 4 bits indicate the message type,
  case data and $F0 of
    DIGITAL_MESSAGE : ProcessDigitalMessage( data and $0F ); // lower 4 bits are the port

    ANALOG_MESSAGE : ProcessAnalogMessage( data and $0F ); // lower 4 bits are the pin

    START_SYSEX : begin
      case data of
        REPORT_VERSION : ProcessReportVersion;
      end; // case
    end; // START_SYSEX
  end; // case
end;


//=====================================================================
//  Procedure: DoErrorEvent
//  Author:    DavidK
//  Date:      14-Oct-2009
//  Comments:
procedure TFirmata.DoErrorEvent(ErrorMsg: string);
begin
  if Assigned( fOnError ) then
    fOnError( self, ErrorMsg );
end;


//=====================================================================
//  Procedure: Reset
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Reset the hardware
procedure TFirmata.Reset;
begin
  if not CommAvailable then
    Exit;

  fCommPort.DoDSRChange(False);
  Sleep( 500 );
  fCommPort.DoDSRChange(True);
end;


//=====================================================================
//  Procedure: Write
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:  Send a FirmataMessage to the hardware
procedure TFirmata.Write( FirmataMsg: TFirmataMessage );
begin
  if DelayWrite then
    Exit;
    
  if CommAvailable then
    fCommPort.Write( FirmataMsg )
  else
    raise EFirmata.Create( 'Unable to update hardware, comm port is not open.');
end;


//=====================================================================
//  Procedure: SetDelayWrite
//  Author:    DavidK
//  Date:      07-Oct-2009
//  Comments:  DelayWrite allows Firmata to control whether
//             changes are written to the hardware.
procedure TFirmata.SetDelayWrite(const Value: Boolean);
begin
  // Rather than using a flag, fDelayWrite is a counter.
  // This allows multiple calls to delay write to overlap.
  if value then
    Inc( fDelayWrite )
  else
    Dec( fDelayWrite );
end;


//=====================================================================
//  Procedure: GetDelayWrite
//  Author:    DavidK
//  Date:      14-Oct-2009
//  Comments:
function TFirmata.GetDelayWrite: Boolean;
begin
  result := fDelayWrite > 0;
end;


//=====================================================================
//  Procedure: SetCommPort
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Reset the comm port
procedure TFirmata.SetCommPort(const Value: string);
begin
  try
    // This is really lazy, but also very effective.
    // If a comm port is already configured, dump it.
    if Assigned( fCommPort ) then
      fCommPort.Free;

    // Create a new comm port
    fCommPort := TFirmataComm.Create( Value, '57600' );
    InitCommPort;
  except
    on E: Exception do begin
      raise EFirmata.CreateFmt( 'Firmata error: %s', [E.message] );
    end;
  end;
end;


//=====================================================================
//  Procedure: GetCommPort
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Return the current port name
function TFirmata.GetCommPort: string;
begin
  if Assigned( fCommPort ) then
    result := fCommPort.Port
  else
    result := 'none';
end;


//=====================================================================
//  Procedure: GetCommBaud
//  Author:    DavidK
//  Date:      20-Aug-2009
//  Comments:  Return the current comm port baud
function TFirmata.GetCommBaud: integer;
begin
  if Assigned( fCommPort ) then
    Result := BaudRateToInt(fCommPort.BaudRate)
  else
    result := 0;
end;


//=====================================================================
//  Procedure: GetDigitalPort
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Property accessor for DigitalPort
function TFirmata.GetDigitalPort(Index:byte): TPort;
begin
  result := fDigitalPorts[Index];
end;


//=====================================================================
//  Procedure: GetDigitalPin
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Property accessor for DigitalPin
function TFirmata.GetDigitalPin(Index: Byte): TDigitalPin;
begin
  result := fDigitalPorts[Index div 8].Pin[Index mod 8];
end;


//=====================================================================
//  Procedure: GetAnalogPin
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Property accessor for AnalogPin. Index indicates the
//             analog pin number, e.g., A0..A5.
function TFirmata.GetAnalogPin(Index:byte): TAnalogPin;
begin
  result := (fDigitalPorts[(Index+AnalogPinsOffset) div 8].Pin[(Index+AnalogPinsOffset) mod 8] as TAnalogPin);
end;


//=====================================================================
//  Procedure: SetAnalogPin
//  Author:    DavidK
//  Date:      20-Aug-2009
//  Comments:  Internal routine for replacing a digital pin with
//             an analog pin. Index indicates the Arduino pin number.
procedure TFirmata.SetAnalogPin(Index: Byte; Pin: TAnalogPin);
begin
  fDigitalPorts[Index div 8].fPins[Index mod 8].Free;
  fDigitalPorts[Index div 8].fPins[Index mod 8] := Pin;
end;



{ TAnalogPin }

//=====================================================================
//  Procedure: Create
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:
constructor TAnalogPin.Create( PortPin: Integer; Port: TPort; AnalogPin: Byte );
begin
  inherited Create( PortPin, Port );
  fAnalogPin := AnalogPin;
  fReporting := False;
  fValue     := 0;
end;


//=====================================================================
//  Procedure: SetReporting
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Controls reporting for the specified pin
procedure TAnalogPin.SetReporting(const Value: Boolean);
begin
  fReporting := Value;
  UpdateReporting;
end;


//=====================================================================
//  Procedure: GetValue
//  Author:    DavidK
//  Date:      14-Oct-2009
//  Comments:  Return the current pin value and clear the Changed flag.
function TAnalogPin.GetValue: Integer;
begin
  Result := fValue;
  fChanged := False;
end;


//=====================================================================
//  Procedure: SetMode
//  Author:    DavidK
//  Date:      12-Oct-2009
//  Comments:
procedure TAnalogPin.SetMode(const Mode: TPinMode);
begin
  case Mode of
    pmAnalogInput   : SetReporting( True );

    // Pass all non-analog modes to the ancester.
    else inherited;
  end; // case
end;


//=====================================================================
//  Procedure: UpdateReporting
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:
procedure TAnalogPin.UpdateReporting;
begin
  fPort.Firmata.Write( TFirmataMessage.ReportAnalogPin( AnalogPin, fReporting ));
end;


{ TDigitalPort }

//=====================================================================
//  Procedure: Create
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:
constructor TPort.Create(Port: Integer; Firmata: TFirmata);
var
  i : Integer;
begin
  inherited Create;

  fFirmata   := Firmata;
  fPort      := Port;
  fReporting := False;

  // Create the TDigitalPin instances that represent each pin
  for i := 0 to high(fPins) do
    fPins[i] := TDigitalPin.Create( i, self );
end;


//=====================================================================
//  Procedure: SetReporting
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Controls reporting for this digital port
procedure TPort.SetReporting(const Value: Boolean);
begin
  fReporting := Value;
  ReportingUpdate;
end;


//=====================================================================
//  Procedure: ReportingUpdate
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:
procedure TPort.ReportingUpdate;
begin
  fFirmata.Write( TFirmataMessage.ReportDigitalPort( Port, fReporting ));
end;


//=====================================================================
//  Procedure: DutyCycleUpdate
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:  Send the current duty cycle configuration to the hardware
procedure TPort.DutyCycleUpdate;
var
  i : Integer;
begin
  for i := 0 to High(fPins) do
    fPins[i].DutyCycleUpdate;
end;


//=====================================================================
//  Procedure: ModeUpdate
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:  Send the current pin mode configuration to the hardware 
procedure TPort.ModeUpdate;
var
  i : Integer;
begin
  for i := 0 to High(fPins) do 
    fPins[i].ModeUpdate;
end;


//=====================================================================
//  Procedure: GetPin
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Property accessor for Pin
function TPort.GetPin(Index: Byte): TDigitalPin;
begin
  result := fPins[Index];
end;


//=====================================================================
//  Procedure: GetChangedPins
//  Author:    DavidK
//  Date:      21-Aug-2009
//  Comments:  Return an array of only the pins marked as changed
function TPort.GetChangedPins: TPinsArray;
var
  i : Integer;
  Changed : Integer;
begin
  Changed := 0;
  // The result variable is a dynamic array which works much
  // like a string. Set it's length to the maximum number of
  // pins that could be changed.
  SetLength( result, High(fPins)+1 );

  // Iterate the array of pins and put a reference to the
  // pins that are flagged as having been changed into the
  // result array.
  for i := 0 to High(fPins) do begin
    if fPins[i].Changed then begin
      Inc(Changed);
      result[i] := fPins[i];
    end;
  end;

  // Trim the length of the result array down to the number
  // of pins that were actually changed.
  SetLength( result, Changed );
end;


//=====================================================================
//  Procedure: SetValue
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Sets the value for the entire port
procedure TPort.SetValue(const Value: byte);
var
  i : integer;
begin
  // Set the delayWrite flag to block the pins from sending their
  // own updates to the comm port.
  delayWrite := True;
  try
    // Iterate the pins and set each output pin state from the
    // appropriate bit in Value.
    for i := 0 to high(fPins) do
      // Mask out the correct bit and assign it to the pin state
      fPins[i].State := ((Value and (1 shl i)) > 0);
  finally
    // Clear the delayWrite flag so updates can go to the comm port
    delayWrite := False;
  end;

  // Send the current value out to the hardware.
  WriteValue;
end;


//=====================================================================
//  Procedure: WriteValue
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Send a message to the hardware to set the ouput pins on
//             this port as they have been specified in the Pin array.
procedure TPort.WriteValue;
var
  i : Integer;
  portVal : byte;
begin
  // The delayWrite flag allows multiple port pins to be changed
  // without sending update commands to the comm port. See SetValue.
  // Note that the TFirmata class also has a DelayWrite property that
  // performs a similar function. Don't confuse the two.
  if delayWrite then
    Exit;

  portVal := 0;

  // Copy pin values into portVal for pins configured as outputs.
  for i := 0 to high(fPins) do
    if fPins[i].Mode = pmDigitalOutput then
      portVal := portVal or (Byte(fPins[i].State) shl i);

  // Send the values for this port out to the hardware
  fFirmata.Write( TFirmataMessage.DigitalIOMessage( Port, portVal ));
end;



{ TDigitalPin }

//=====================================================================
//  Procedure: Create
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:
constructor TDigitalPin.Create( PortPin: Byte; Port: TPort );
begin
  inherited Create;

  fPort     := Port;
  fPortPin  := PortPin;
  fState    := False;
  fMode     := pmDigitalInput;
  fBoardPin := (fPort.Port * 8) + fPortPin;
end;


//=====================================================================
//  Procedure: SetMode
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Set the mode of operation for the pin
procedure TDigitalPin.SetMode(const Mode: TPinMode);
begin
  if (Mode = pmDigitalPWM)
  and not (BoardPin in PWM_PINS) then
    raise Exception.CreateFmt('Digital pin %d does not have PWM capabilities', [BoardPin]);

  if (Mode = pmUnavailable) then
    raise Exception.CreateFmt( 'Cannot set mode for pin %d', [BoardPin]);

  fMode := Mode;
  ModeUpdate;
end;


//=====================================================================
//  Procedure: SetState
//  Author:    DaveK
//  Date:      16-Aug-2009
//  Comments:  Set the state of this pin
procedure TDigitalPin.SetState(const State: Boolean);
begin
  if fState = State then
    Exit;

  case Mode of
    // If the pin mode is digital input, save the state that
    // is being set and also set the 'changed' flag to indicate
    // that this bit has been updated.
    pmDigitalInput : begin
      fState := State;
      fChanged := True;
    end;

    // If the pin mode is digital output, save the state and
    // also indicate to the port that the value needs to be
    // written out to the hardware.
    pmDigitalOutput : begin
      fState := State;
      fPort.WriteValue;
    end;

    pmAnalogInput :
      raise EFirmata.CreateFmt( 'Digital pin %d is not an analog input pin', [BoardPin]);

    pmDigitalPWM :
      raise EFirmata.Create( 'Set Digital PWM value with DutyCycle instead of State' );

    pmServo :
      raise EFirmata.Create( 'Servo mode not yet supported' );

    pmUnavailable :
      raise EFirmata.CreateFmt( 'Cannot set the state of pin %d', [BoardPin]);

  end;
end;


//=====================================================================
//  Procedure: GetState
//  Author:    DavidK
//  Date:      14-Oct-2009
//  Comments:
function TDigitalPin.GetState: Boolean;
begin
  result := fState;
  fChanged := False;
end;


//=====================================================================
//  Procedure: SetDutyCycle
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Set the duty cycle for a PWM pin
procedure TDigitalPin.SetDutyCycle(const Value: Byte);
begin
  if fDutycycle = Value then
    Exit;

  if Mode <> pmDigitalPWM then
    raise Exception.CreateFmt( 'Digital pin %d is not configured as a Digital PWM output', [BoardPin]);

  fDutyCycle := Value;
  DutyCycleUpdate;
end;


//=====================================================================
//  Procedure: ModeUpdate
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:  Configure the hardware with the current pin mode.
procedure TDigitalPin.ModeUpdate;
begin
  // The port will handle getting the message to the hardware
  fPort.Firmata.Write( TFirmataMessage.SetPinMode(BoardPin, Mode));
end;


//=====================================================================
//  Procedure: DutyCycleUpdate
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:  Configure the hardware with the current duty cycle
procedure TDigitalPin.DutyCycleUpdate;
begin
  // The port will handle getting the message to the hardware
  fPort.Firmata.Write( TFirmataMessage.AnalogIOMessage(BoardPin, fDutyCycle) );
end;


{ TDuemilanoveFirmata }

//=====================================================================
//  Procedure: DuemilanoveSetup
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:  Setup steps specific to the Duemilanove.
procedure TDuemilanoveFirmata.DuemilanoveSetup;
begin
  // Make the serial pins unavailable
  DigitalPin[0].fMode := pmUnavailable; // serial TX
  DigitalPin[1].fMode := pmUnavailable; // serial RX
end;


//=====================================================================
//  Procedure: Create
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:  Constructor that supports commport configuration
constructor TDuemilanoveFirmata.Create(CommPort: string; Baud: string = '57600');
begin
  inherited Create( 20, 6, 14, TFirmataComm.Create(CommPort, Baud));
  DuemilanoveSetup;
end;


//=====================================================================
//  Procedure: Create
//  Author:    DavidK
//  Date:      02-Oct-2009
//  Comments:  Constructor without comm port configuration. The port
//             must be configured before the object can be used.
constructor TDuemilanoveFirmata.Create;
begin
  inherited Create( 20, 6, 14, nil );
  DuemilanoveSetup;
end;

end.
