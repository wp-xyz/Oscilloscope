unit oUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, IniFiles, Dialogs,
  ueSelector,
  oGlobal;

function CreateIni: TCustomIniFile;

function CheckboxMessageDlg(const AMsg, ACheckboxMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint;
  out IsChecked: Boolean): Integer;

procedure BoldRadioGroup(AControl: TRadiogroup);

function GetTextWidth(AText: String; AFont: TFont): Integer;

procedure PopulateSensitivity(ASelector: TueSelector);

procedure WriteWavStream(AStream: TStream; Frequency, Amplitude: Double;
  ADuration, ASampleRate, AChannel: Integer; AWaveForm: TWaveForm;
  ADutyCycle: Double);

implementation

uses
  Controls, StdCtrls, Forms,
  oWav;

function CreateIni: TCustomIniFile;
begin
  Result := TMemIniFile.Create(GetAppConfigFile(false));
end;

function CheckboxMessageDlg(const AMsg, ACheckboxMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint;
  out IsChecked: Boolean): Integer;
var
  F: TForm;
  cb: TCheckbox;
begin
  F := CreateMessageDialog(AMsg, DlgType, Buttons);
  F.Height := F.Height + 16;
  cb := TCheckbox.Create(F);
  cb.Parent := F;
  cb.left := 8;
  cb.Top := F.Height - 24;
  cb.Caption := ACheckboxMsg;
  Result := F.ShowModal;
  IsChecked := (Result <> mrCancel) and cb.Checked;
end;

{ Set font style of RadioGroup caption to bold, but keep items normal }
procedure BoldRadioGroup(AControl: TRadiogroup);
var
  i: Integer;
begin
  AControl.Font.Style := [fsBold];
  for i:=0 to AControl.ControlCount-1 do
    AControl.Controls[i].Font.Style := [];
end;

function GetTextWidth(AText: String; AFont: TFont): Integer;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.SetSize(1, 1);
  bmp.Canvas.Font.Assign(AFont);
  Result := bmp.Canvas.TextWidth(AText);
  bmp.Free;
end;

procedure PopulateSensitivity(ASelector: TuESelector);
var
  i: Integer;
  L: TStrings;
begin
  L := TStringList.Create;
  try
    for i:=0 to High(SENSITIVITIES) do
      L.Add(Format('%.0f%%', [SENSITIVITIES[i]]));
    ASelector.Items.Assign(L);
  finally
    L.Free;
  end;
  ASelector.Index := ASelector.Items.Count - 1;
end;

{ Frequency in Hz, Amplitude = 0..1, ADuration in s, ASampleRate in samples/sec
  AChannel = 0 or 3 --> mono, 1 --> left only, 2 --> right only }
procedure WriteWavStream(AStream: TStream; Frequency, Amplitude: Double;
  ADuration, ASampleRate, AChannel: Integer; AWaveForm: TWaveForm;
  ADutyCycle: Double);
const
  BITS_PER_SAMPLE = 16;
  MAX_WAV = 32760;
  MIN_WAV = -MAX_WAV;
var
  i: Integer;
  n: Integer;  // Number of samples
  p: Int64;
  t, omega: Double;
  phi, phiDC: Double;
  two_pi: Double;
  numch: Integer;

  procedure WriteValue(phi, Ampl: Double);
  var
    y: Double;
    value: SmallInt;
  begin
    case AWaveForm of
      wfSine : y := Ampl*sin(phi);
      wfTri  : if phi <= phiDC then
                 y := 2*Ampl*phi/phiDC - Ampl else
                 y := Ampl*(two_pi+phiDC-phi*2)/(two_pi-phiDC);
      wfRect : if phi <= phiDC then
                 y := Ampl else
                 y := -Ampl;
    end;
    value := round(y*MAX_WAV);
    if value > MAX_WAV then value := MAX_WAV;
    if value < MIN_WAV then value := MIN_WAV;
    AStream.WriteWord(word(value));
  end;

  procedure WriteData;
  var
    i: Integer;
    phi: Double;
  begin
  end;

begin
  if abs(Amplitude) > 1.0 then
    raise Exception.Create('Wav writer: amplitude must be <= 1.0');

  if not (AChannel in [0..3]) then
    raise Exception.Create('Wav writer: 1 or 2 channels supported only.');

  if (ADutyCycle <= 0.0) or (ADutyCycle >= 1.0) then
    raise Exception.Create('Wav writer: Duty cycle must be > 0 and < 1');

  if AChannel in [0, 3] then numch := 1 else numch := 2;

  two_pi := 2.0 * pi;
  WriteWavHeader(AStream, numch, ASampleRate, BITS_PER_SAMPLE);
  p := AStream.Position;  // remember begin of data section

  omega := two_pi * Frequency;
  phiDC := ADutyCycle * two_pi;
  n := round(ADuration * ASampleRate);

  for i:=0 to n-1 do begin
    t := ADuration * i / n;                      // time in s
    phi := omega*t;                              // phase angle ...
    phi := phi - trunc(phi/two_pi)*two_pi;       // ... in range 0..2pi
    if numch = 1 then
      WriteValue(phi, Amplitude)
    else
      case AChannel of
        1: // left channel only
          begin
            WriteValue(phi, Amplitude);  // L
            WriteValue(phi, 0);          // R
          end;
        2: // right channel only
          begin
            WriteValue(phi, 0);         // L
            WriteValue(phi, Amplitude); // R
          end;
      end;
  end;

  // Complete missing header data
  AStream.Position := p - 4;
  AStream.WriteDWord(n * numch * BITS_PER_SAMPLE); // Size of data part
  AStream.Position := 4;
  AStream.WriteDWord(AStream.Size - 8);  // File size - RIFF chunk size
end;


end.

