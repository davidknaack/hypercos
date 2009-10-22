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

unit formMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, StdCtrls, Buttons, uHyperCos, uFirmata;

type
  TfrmMain = class(TForm, IHyperCosDisplay)
    Image1: TImage;
    Label4: TLabel;
    Label5: TLabel;
    lblLastCommand: TLabel;
    lblFirmataInfo: TLabel;
    lblErrorText: TLabel;
    lblScratchInfo: TLabel;
    lblManual: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lblErrorTextClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    slCmdHistory : TStringList;
    cmdHistFile : string;
    HistoryPos : Integer;
    fHyperCos : IHyperCosControl;
    ButtonBits : array of TSpeedButton;
    IOLabels : array of TLabel;
    procedure ButtonBitClick(Sender: TObject);
    procedure AppOnExcept(Sender: TObject; E: Exception);

    // IHyperCosDisplay implementations
    procedure ShowFirmataConnectionStatus( const Port: string; const Speed: Integer; const Version: string; const Connected: Boolean );
    procedure ShowScratchConnectionStatus( const Host: string; Port: Integer; const Connected: Boolean );
    procedure ShowErrorText( const Text: string );
    procedure ShowLastCmdText( const Text: string );
    procedure ShowPinState( const Pin: Integer; const State: Boolean );
    procedure ShowPinMode( const Pin: Integer; const Mode: TPinMode );
    function GetHyperCos: IHyperCosControl;
  public
    property HyperCos: IHyperCosControl read GetHyperCos write fHyperCos;
  end;

var
  frmMain: TfrmMain;

implementation

uses Math;

{$R *.dfm}


{ TfrmMain }


//=====================================================================
//  Procedure: FormCreate
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  Form creation, set up connections, controls, etc.
procedure TfrmMain.FormCreate(Sender: TObject);
const
  cControlsTop = 30;
  cControlsLeft = 70;
var
  i : Integer;
begin
  Application.OnException := AppOnExcept;

  // Instead of setting up the visual controls in the form designer, this
  // code is used to create them at start-up. This is a little more typing,
  // but it's much easier to manage an array of similar controls this way.
  SetLength( ButtonBits, 14 );
  SetLength( IOLabels, 14 );
  for i := 0 to 13 do begin
    // Labels displaying the pin mode
    IOLabels[i] := TLabel.Create(Self);
    with IOLabels[i] do
    begin
      AutoSize := False;
      Parent := Self;
      Width := 15;
      Height := 13;
      Left := cControlsLeft + (i*(Width+4));
      Top := cControlsTop;
      Caption := 'o';
      ParentFont := False;
      Font.Color := clWhite;
      Font.Style := [fsBold];
      Transparent := True;
      Alignment := taCenter;
    end;

    // Create a 14 buttons representing the Arduino pins
    ButtonBits[i] := TSpeedButton.Create(Self);
    with ButtonBits[i] do
    begin
      Parent := Self;
      Tag := i;
      Width := 17;
      Height := 17;
      Left := cControlsLeft + (i*(Width + 2));
      Top := cControlsTop + 16;
      GroupIndex := Tag;
      AllowAllUp := True;
      Caption := 'D';
      OnClick := ButtonBitClick;
    end;

    // Labels identifying the buttons, 2..13
    with TLabel.Create(Self) do
    begin
      AutoSize := False;
      Parent := Self;
      Width := 15;
      Height := 13;
      Left := cControlsLeft + (i*(Width+4));
      Top := cControlsTop + 33;
      Caption := IntToStr( i );
      ParentFont := False;
      Font.Color := clWhite;
      Font.Style := [fsBold];
      Transparent := True;
      Alignment := taCenter;
    end;
  end;

  // This form represents a Duemilanove compatible device,
  // so disable the first two pins. They are used for serial
  // IO and are not available.
  ButtonBits[0].Enabled := False;
  ButtonBits[1].Enabled := False;
  IOLabels[0].Caption := 'x';
  IOLabels[1].Caption := 'x';

  // A stringlist for use as a simple command buffer.
  slCmdHistory := TStringList.Create;
  cmdHistFile := ExtractFilePath( Application.ExeName ) + 'CmdHistory.txt';
  if FileExists( cmdHistFile ) then
    slCmdHistory.LoadFromFile( cmdHistFile );
end;


//=====================================================================
//  Procedure: FormDestroy
//  Author:    DavidK
//  Date:      21-Oct-2009
//  Comments:
procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Most of the objects created in the FormCreate are owned by
  // the form and so will automatically be released. The command
  // buffer will have to be released here.
  slCmdHistory.Free;
end;


//=====================================================================
//  Procedure: GetHyperCos
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Property read accessor for HyperCosControl interface.
//             The form will use this internally rather than accessing
//             the field directly in order to protect itself from an
//             unset control interface.
function TfrmMain.GetHyperCos: IHyperCosControl;
begin
  Result := fHyperCos;
  if Result = nil then
    raise Exception.Create( 'HyperCos control interface has not been set.' );
end;


//=====================================================================
//  Procedure: ShowFirmataConnectionStatus
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:
procedure TfrmMain.ShowFirmataConnectionStatus( const Port: string; const Speed: Integer; const Version: string; const Connected: Boolean );
var conn: string;
begin
  if Connected then begin
    conn := 'connected';
    // Clear any errors
    ShowErrorText('');
  end else
    conn := 'disconnected';

  lblFirmataInfo.Caption := Format( 'Firmata: %s:%d %s %s', [Port, Speed, Version, conn])
end;


//=====================================================================
//  Procedure: ShowScratchConnectionStatus
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:
procedure TfrmMain.ShowScratchConnectionStatus( const Host: string; Port: Integer; const Connected: Boolean );
var conn: string;
begin
  if Connected then begin
    conn := 'connected';
    // Clear any errors
    ShowErrorText('');
  end else
    conn := 'disconnected';

  lblScratchInfo.Caption := Format( 'Scratch: %s:%d %s', [Host, Port, conn]);
end;


//=====================================================================
//  Procedure: ShowErrorText
//  Author:    DavidK
//  Date:      07-Oct-2009
//  Comments:  Display the error message on the form and manage
//             showing and hiding the error label. If the label
//             is not hidden when it is blank it's hint will show
//             up when the mouse hovers over it.
procedure TfrmMain.ShowErrorText( const Text: string );
begin
  lblErrorText.Caption := Text;
  lblErrorText.Visible := lblErrorText.Caption <> '';
end;


//=====================================================================
//  Procedure: ShowLastCmdText
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:
procedure TfrmMain.ShowLastCmdText( const Text: string );
begin
  lblLastCommand.Caption := Text;
end;


//=====================================================================
//  Procedure: ShowPinState
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Handle notification from HyperCos of pin state changes.
procedure TfrmMain.ShowPinState(const Pin: Integer; const State: Boolean);
begin
  ButtonBits[Pin].Down := State;
end;


//=====================================================================
//  Procedure: ShowPinMode
//  Author:    DavidK
//  Date:      08-Oct-2009
//  Comments:  Handle notification from HyperCos of pin mode changes.
procedure TfrmMain.ShowPinMode(const Pin: Integer; const Mode: TPinMode);
var
  cap : string;
begin
  // Display an indicator for each mode.
  case Mode of
    pmDigitalInput  : cap := 'i';
    pmDigitalOutput : cap := 'o';
    pmAnalogInput   : cap := 'a';
    pmDigitalPWM    : cap := 'p';
    pmServo         : cap := 's';
    pmUnavailable   : cap := '-';
    else              cap := '?';
  end;

  IOLabels[Pin].Caption := cap;
end;


//=====================================================================
//  Procedure: AppOnExcept
//  Author:    DavidK
//  Date:      07-Oct-2009
//  Comments:  Unhandled exceptions may come through here, depending
//             on whether a third-party exception handler is installed.
//             Packages like madExcept or the JCL Debug system can catch
//             exceptions before they arrive here.
//             Most of the code in this form that is be expected to raise
//             various exceptions is trapped and redirected already.
procedure Tfrmmain.AppOnExcept(Sender: TObject; E: Exception);
begin
  ShowErrorText( E.Message );
end;


//=====================================================================
//  Procedure: lblErrorTextClick
//  Author:    DavidK
//  Date:      03-Oct-2009
//  Comments:  Clear error message when clicked
procedure TfrmMain.lblErrorTextClick(Sender: TObject);
begin
  ShowErrorText( '' );
end;


//=====================================================================
//  Procedure: ButtonBitClick
//  Author:    DavidK
//  Date:      17-Aug-2009
//  Comments:  The user has clicked one of the buttons representing
//             a port pin. Set the state of the appropriate pin.
procedure TfrmMain.ButtonBitClick(Sender: TObject);
const
  cPinState : array[boolean] of string = ('low', 'high');
begin
  try
    with Sender as TSpeedButton do
      // For simplicity HyperCos currently just allows for the injection
      // of text as if it had been sent from Scratch (minus the caret prefix).
      HyperCos.InjectCommand( Format( 'digitalwrite %d %s', [Tag, cPinState[Down]]));
  except
    on e:Exception do
      ShowErrorText( E.Message );
  end;
end;


//=====================================================================
//  Procedure: FormKeyUp
//  Author:    DavidK
//  Date:      21-Oct-2009
//  Comments:  This is a simple debug feature. The user may type
//             HyperCos commands directly into the interface much
//             as if Scratch had sent them. Note that the caption
//             property is used as a storage variable for the user's
//             input. In general this is a very bad habit to get into.
//             It combines data management with the user interface
//             elements and makes it difficult to reuse or refactor.
//             I'm doing it here only because this is intended as a
//             simple debug feature. However, you can see that feature
//             creep has already started with the addition of a command
//             buffer to allow easy reuse of commands. It might be a
//             good exercise to create a new form that replaces this
//             badly designed bit of code with a nicer interface that
//             give the user some hints about what commands can be used,
//             maybe some buttons for quick access, etc.
procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  oldColor : TColor;
begin
  case key of
    // Append keystrokes to command buffer
    // The calls to Ord() here are just to make it easy to use
    // the VK_ codes below which are integer type. You could also
    // leave out the Ord()s here and cast the VK_ constants to char.
    Ord('A')..Ord('Z'),
    Ord('0')..Ord('9'),
    Ord(' ') :
      lblManual.Caption := lblManual.Caption + Char(Key);


    // Up arrow. 'VK' stands for 'virtual key'.
    VK_Up : begin
      // Decrement and limit history pointer
      Dec( HistoryPos );
      HistoryPos := Max( HistoryPos, 0 );

      // Display the command
      lblManual.Caption := slCmdHistory[HistoryPos];
    end;


    // Down arrow
    VK_Down : begin
      // Increment and limit history pointer
      Inc( HistoryPos );
      HistoryPos := Min( HistoryPos, slCmdHistory.Count-1 );

      // Display the command
      lblManual.Caption := slCmdHistory[HistoryPos];
    end;


    // Typos happen.
    VK_BACK :
      lblManual.Caption := copy( lblManual.Caption, 1, length( lblManual.Caption )-1);

    // Escape key clears input
    VK_ESCAPE :
      lblManual.Caption := '';


    // Enter submits input
    VK_RETURN : begin
      // Manage the command history
      if slCmdHistory.IndexOf( lblManual.Caption ) = -1 then begin
        slCmdHistory.Add( lblManual.Caption );
        // Save it now, during development it isn't unusual to kill
        // the program without going through normal shutdown, so if
        // the file is only saved then it won't ever build the history.
        slCmdHistory.SaveToFile( cmdHistFile );
      end;

      // Some commands take a little while to process, so provide some
      // feedback by changing the label color.
      oldColor := lblManual.font.Color;
      lblManual.font.Color := clLime;
      lblManual.Refresh;
      // inject the command
      HyperCos.InjectCommand( lblManual.Caption );
      // Reset the label color.
      lblManual.Caption := '';
      lblManual.font.Color := oldColor;
    end;
  end; // case
end;


end.
