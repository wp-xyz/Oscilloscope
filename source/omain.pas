unit oMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,  //lazlogger,
  Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, ComCtrls, Buttons, Spin,
  uEKnob, uESelector, ueled,
  oGlobal, oDataCollector, oBaseFrame;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    BtnSineWave: TSpeedButton;
    BtnSquareWave: TSpeedButton;
    BtnTriangWave: TSpeedButton;
    CbRecorderSampleRate: TComboBox;
    CbGeneratorSampleRate: TComboBox;
    CbAudioEngine: TComboBox;
    DataPanelBevel: TBevel;
    BtnStart: TButton;
    BtnStop: TButton;
    CbInputDevice: TComboBox;
    DataPanelBevel1: TBevel;
    EdDutyCycle: TSpinEdit;
    EdFrequency: TFloatSpinEdit;
    EdVolume: TSpinEdit;
    GbDutyCycle: TGroupBox;
    GbVolume: TGroupBox;
    GbWaveForm: TGroupBox;
    HeaderPanel1: TPanel;
    Label2: TLabel;
    LblKHz: TLabel;
    GbFrequency: TGroupBox;
    InfoRecordingLevel: TLabel;
    Label1: TLabel;
    LblDeviceType: TLabel;
    LblInputDevice: TLabel;
    LblDutyCyclePerc: TLabel;
    LblVolPerc: TLabel;
    LblRecorderSampleRate: TLabel;
    LblGeneratorSampleRate: TLabel;
    InputNotebook: TNotebook;
    RecorderPage: TPage;
    PlayerPage: TPage;
    WaveGeneratorPage: TPage;
    PageControl: TPageControl;
    MeasurementPanel: TPanel;
    InputSplitter: TSplitter;
    FrequencyPanel: TPanel;
    StartStopPanel: TPanel;
    PotFrequency: TuEKnob;
    SwFrequency: TuESelector;
    TbDutyCycle: TTrackBar;
    TbVolume: TTrackBar;
    RunLED: TuELED;
    WaveGeneratorPanel: TPanel;
    PlayerPanel: TPanel;
    RecorderPanel: TPanel;
    SourcePanel: TPanel;
    HeaderPanel: TPanel;
    CbSource: TComboBox;
    EdFilename: TFileNameEdit;
    LblSource: TLabel;
    LblFile: TLabel;
    LblRecordLevel: TLabel;
    InputPanel: TPanel;
    PgOscilloscope: TTabSheet;
    PgSpectrum: TTabSheet;
    TbRecordingLevel: TTrackBar;

    procedure BtnSineWaveClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure CbAudioEngineChange(Sender: TObject);
    procedure CbInputDeviceSelect(Sender: TObject);
    procedure CbSourceSelect(Sender: TObject);
    procedure CbVolumeLeftONClick(Sender: TObject);
    procedure CbGeneratorSampleRateSelect(Sender: TObject);

    procedure EdDutyCycleChange(Sender: TObject);
    procedure EdFilenameAcceptFileName(Sender: TObject; var Value: String);
    procedure EdFrequencyChange(Sender: TObject);
    procedure EdVolumeChange(Sender: TObject);

    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PotFrequencyChange(Sender: TObject);
    procedure SwFrequencyChange(Sender: TObject);

    procedure TbDutyCycleChange(Sender: TObject);
    procedure TbRecordingLevelChange(Sender: TObject);
    procedure TbVolumeChange(Sender: TObject);

  private
    { private declarations }
    FFileName: String;
    FFrames: array[0..1] of TBaseFrame;
    FCurrentFrame: TBaseFrame;
    FDutyCycleLock: Integer;
    FFrequencyLock: Integer;
    FVolumeLock: Integer;
    FGeneratorStream: TMemoryStream;
    function GetSampleRate: Integer;
    procedure PlaybackBeginHandler(Sender: TObject);
    procedure PlaybackDataReceivedHandler(Sender: TObject);
    procedure PlaybackEndHandler(Sender: TObject);
    procedure PopulateFrequencies;
    procedure PopulateSampleRates(ACombo: TCombobox);
    procedure SelectAudioEngine(AudioEngineName: String);
    procedure UpdateCaption;
    procedure UpdateFrequency;
    procedure UpdateInputInfo;
    procedure UpdateWav;

  protected
    procedure ReadIni;
    procedure WriteIni;

  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math, IniFiles,
  oUtils, oBASSDataCollector, ouosDataCollector, oOscilloscopeFrame, oSpectrumFrame;


{ TMainForm }

procedure TMainForm.BtnSineWaveClick(Sender: TObject);
begin
  UpdateWav;
end;

procedure TMainForm.BtnStartClick(Sender: TObject);
var
  ch: Integer;
  wf: TWaveForm;
  ok: Boolean;
begin
  if not Assigned(DataCollector) then
    exit;

  if DataCollector.Running then    // running --> pause
  begin
    FCurrentFrame.Pause;
    BtnStart.Caption := 'Resume';
    RunLED.Color := clGreen;
  end else
  if DataCollector.Paused then     // paused --> continue
  begin
    FCurrentFrame.Resume;
    BtnStart.Caption := 'Pause';
    RunLED.Color := clLime;
  end else
  begin                            // not yet started --> start
    case CbSource.ItemIndex of
      0: begin
           if not NoDamageWarning_Mic and
             (CheckboxMessageDlg('WARNING:'#13'This function may damage your soundcard '+
               'if the input signal is too high.', 'Don''t show this message again.',
               mtWarning, [mbOK, mbCancel], 0, NoDamageWarning_Mic) <> mrOK) then exit;
           ok := FCurrentFrame.StartRecording(SAMPLE_RATES[CbRecorderSampleRate.ItemIndex]);
         end;
      1: ok := FCurrentFrame.StartPlayback(FFileName);
      2: begin
           if not NoDamageWarning_Out and
             (CheckboxMessageDlg('WARNING:'#13'This function may damage your speakers '+
             'if the output signal is too high, especially at high frequencies.',
             'Don''t show this message again',
             mtWarning, [mbOK, mbCancel], 0, NoDamageWarning_Out) <> mrOK) then exit;
        {
           if CbVolumeLeftON.Checked and not CbVolumeRightOn.Checked then ch := 1
             else if not CbVolumeLeftON.Checked and CbVolumeRightON.Checked then ch := 2
               else if CbVolumeLeftON.Checked and CbVolumeRightON.Checked then ch := 3
                 else exit;
                 }
           ch := 3;  // mono
           if BtnSineWave.Down then wf := wfSine
             else if BtnTriangWave.Down then wf := wfTri
               else if BtnSquareWave.Down then wf := wfRect;
           FGeneratorStream.Clear;
           WriteWavStream(FGeneratorStream, EdFrequency.Value*1000, EdVolume.Value/100,
             10, SAMPLE_RATES[CbGeneratorSampleRate.ItemIndex], ch, wf, EdDutyCycle.Value/100);
           ok := FCurrentFrame.StartPlayback(FGeneratorStream.Memory, FGeneratorStream.Size, true);
         end;
    end;
    if ok then
    begin
      BtnStart.Caption := 'Pause';
      BtnStop.Enabled := true;
      RunLED.Color := clLime;
      RunLED.Active := true;
    end;
//    if not ok then BtnStopClick(nil);
  end;
end;

procedure TMainForm.BtnStopClick(Sender: TObject);
begin
  FCurrentFrame.Stop;
  BtnStop.Enabled := false;
  BtnStart.Caption := 'Start';
  RunLED.Active := false;
  Application.ProcessMessages;
end;

procedure TMainForm.CbAudioEngineChange(Sender: TObject);
begin
  SelectAudioEngine(CbAudioEngine.Text);
end;

procedure TMainForm.CbGeneratorSampleRateSelect(Sender: TObject);
begin
  UpdateWav;
end;

{ Enable the selected input }
procedure TMainForm.CbInputDeviceSelect(Sender: TObject);
var
  i: Integer;
begin
  if DataCollector = nil then
    exit;

  // first disable all inputs ...
  for i:=0 to CbInputDevice.Items.Count-1 do
    DataCollector.SetRecordingInputActive(i, false);
  // ... then enable the selected one.
  DataCollector.SetRecordingInputActive(CbInputDevice.ItemIndex, true);
  // Update info
  UpdateInputInfo;
end;

procedure TMainForm.CbSourceSelect(Sender: TObject);
var
  pName: PAnsiChar;
  i: Integer;
  level: Single;
begin
  RecorderPanel.Hide;
  PlayerPanel.Hide;
  WaveGeneratorPanel.Hide;

  case TInputMode(CbSource.ItemIndex) of
    imRecorder:
      begin
        RecorderPanel.Top := SourcePanel.Height;
        RecorderPanel.Show;
        if Assigned(DataCollector) then
          DataCollector.GetRecordingDeviceList(CbInputDevice.Items, i);
        CbInputDevice.ItemIndex := i;
        CbInputDeviceSelect(Self);  // display info
        UpdateCaption;
      end;

    imPlayer:
      begin
        PlayerPanel.Top := SourcePanel.Height;
        PlayerPanel.Show;
        UpdateCaption;
      end;

    imWaveGenerator:
      begin
        WaveGeneratorPanel.Top := SourcePanel.Height;
        WaveGeneratorPanel.Show;
        UpdateCaption;
      end;
  end;
end;

procedure TMainForm.CbVolumeLeftONClick(Sender: TObject);
begin
  UpdateWav;
end;

procedure TMainForm.EdDutyCycleChange(Sender: TObject);
begin
  if FDutyCycleLock > 0 then
    exit;
  inc(FDutyCycleLock);
  TbDutyCycle.Position := EdDutyCycle.Value;
  UpdateWav;
  dec(FDutyCycleLock);
end;

procedure TMainForm.EdFilenameAcceptFileName(Sender: TObject; var Value: String);
begin
  FFileName := Value;
  Value := ExtractFilename(FFileName);
  EdFilename.InitialDir := ExtractFileDir(FFileName);  // for next call
end;

procedure TMainForm.EdFrequencyChange(Sender: TObject);
var
  mant,ex: Double;
  s: String;
  i: Integer;
begin
  if FFrequencyLock > 0 then
    exit;
  inc(FFrequencyLock);

  Str(EdFrequency.Value, s);   // s is string value in kHz
  Val(Copy(s, 1, Pos('E', s)-1), mant, i);
  Val(Copy(s, Pos('E', s)+1, MaxInt), ex, i);
  while Power(10, ex+3) < FREQUENCIES[0] do begin
    ex := ex + 1;
    mant := mant / 10;
  end;
  while Power(10, ex+3) > FREQUENCIES[High(FREQUENCIES)] do begin
    ex := ex - 1;
    mant := mant * 10;
  end;
  for i:=0 to High(FREQUENCIES[i]) do
    if Power(10, ex+3) = FREQUENCIES[i] then
    begin
      SwFrequency.Index := i;
      break;
    end;
  PotFrequency.Position := mant;

  UpdateWav;

  dec(FFrequencyLock);
end;

procedure TMainForm.EdVolumeChange(Sender: TObject);
begin
  if FVolumeLock > 0 then
    exit;
  inc(FVolumeLock);
  TbVolume.Position := EdVolume.Value;
  UpdateWav;
  dec(FVolumeLock);
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  w, wk: Integer;
begin
  w := MaxValue([BtnStart.Width, BtnStop.Width]);
  BtnStart.Width := w;
  BtnStop.Width := w;

  with SwFrequency do
    wk := DefKnobRadius + 2*LTicksSize + 2*GetTextWidth('100k', SwFrequency.ValuesFont) + 2*SwFrequency.ValuesMargin;

  BtnSineWave.Constraints.MinHeight := BtnStart.Height;
  BtnTriangWave.Constraints.MinHeight := BtnStart.Height;
  BtnSquareWave.Constraints.MinHeight := BtnStart.Height;
  w := MaxValue([BtnSineWave.Width, BtnTriangWave.Width, BtnSquareWave.Width]);
  InputPanel.Constraints.MinWidth := MaxValue([
    RunLED.Width + 2*BtnStart.Width + 4*BtnStart.BorderSpacing.Left,
    3*w + GbWaveForm.ChildSizing.HorizontalSpacing*2 + GbWaveForm.ChildSizing.LeftRightSpacing,
    2*wk + Bevel3.Width
  ]);
  GbWaveForm.ChildSizing.Layout := cclTopToBottomThenLeftToRight;

  FFrames[0].AutoSizeControls;
  FFrames[1].AutoSizeControls;

  if TInputMode(CbSource.ItemIndex) = imRecorder then
    CbSourceSelect(nil);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  ci: TChannelIndex;
begin
  (*
   DataCollector := TBassDataCollector.Create(Handle);
  if not DataCollector.Open(44100, 2) then begin
    MessageDlg(DataCollector.ErrMsg, mtError, [mbOK], 0);
   // Halt;
  end;
  *)

  FFrames[0] := TOscilloscopeFrame.Create(self);
  FFrames[0].Parent := PgOscilloscope;
  FFrames[0].Align := alClient;
  FFrames[0].SetDataCollector(DataCollector);
  FFrames[0].SetSampleRate(GetSampleRate);
  FFrames[0].OnBeginPlayback := @PlaybackBeginHandler;
  FFrames[0].OnEndPlayback := @PlaybackEndHandler;
  FFrames[0].OnDataReceived := @PlaybackDataReceivedHandler;

  FFrames[1] := TSpectrumFrame.Create(self);
  FFrames[1].Parent := PgSpectrum;
  FFrames[1].Align := alClient;
  FFrames[1].SetDataCollector(DataCollector);
  FFrames[1].SetSampleRate(GetSampleRate);
  FFrames[1].OnBeginPlayback := @PlaybackBeginHandler;
  FFrames[1].OnEndPlayback := @PlaybackEndHandler;
  FFrames[1].OnDataReceived := @PlaybackDataReceivedHandler;

  ReadIni;

  PopulateSampleRates(CbRecorderSampleRate);
  PopulateSampleRates(CbGeneratorSampleRate);
  PopulateFrequencies;
  EdFilename.InitialDir := '';
  if EdFilename.Filename <> '' then
    FFileName := ExpandFilename(EdFilename.FileName);

  PlayerPanel.Parent := InputPanel;
  PlayerPanel.Align := alClient;

  RecorderPanel.Parent := InputPanel;
  RecorderPanel.Align := alClient;

  WaveGeneratorPanel.Parent := InputPanel;
  WaveGeneratorPanel.Align := alClient;

  CbSourceSelect(nil);
  PageControlChange(nil);

  FGeneratorStream := TMemoryStream.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  try
    WriteIni;
  except
  end;
  if Assigned(DataCollector) then
  begin
    DataCollector.Close;
    DataCollector.Free;
  end;
  FGeneratorStream.Free;
end;

function TMainForm.GetSampleRate: Integer;
begin
  if (CbRecorderSampleRate.ItemIndex >= 0) and
     (CbRecorderSampleRate.ItemIndex < Length(SAMPLE_RATES))
  then
    Result := SAMPLE_RATES[CbRecorderSampleRate.ItemIndex] else
    Result := 0;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  FCurrentFrame := FFrames[PageControl.ActivePageIndex];
  if FCurrentFrame <> nil then FCurrentFrame.Activate;
  UpdateCaption;
end;

procedure TMainForm.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if FCurrentFrame <> nil then
    FCurrentFrame.Deactivate;
end;

procedure TMainForm.PotFrequencyChange(Sender: TObject);
begin
  if FFrequencyLock > 0 then
    exit;
  inc(FFrequencyLock);
  UpdateFrequency;
  UpdateWav;
  dec(FFrequencyLock);
end;

procedure TMainForm.PlaybackDataReceivedHandler(Sender: TObject);
begin
  //
end;

procedure TMainForm.PlaybackBeginHandler(Sender: TObject);  
begin
  //
end;

procedure TMainForm.PlaybackEndHandler(Sender: TObject);
begin
  BtnStop.Enabled := false;
  BtnStart.Caption := 'Start';
  RunLED.Active := false;
end;

procedure TMainForm.PopulateFrequencies;
var
  i: Integer;
  L: TStringList;
begin
  L := TStringList.Create;
  try
    for i:=0 to High(FREQUENCIES) do
      if FREQUENCIES[i] < 1000 then
        L.Add(IntToStr(FREQUENCIES[i])) else
        L.Add(Format('%.0nk', [FREQUENCIES[i]/1000]));
    SwFrequency.Items.Assign(L);
    SwFrequency.Index := SwFrequency.Items.IndexOf('1k');
  finally
    L.Free;
  end;
end;

procedure TMainForm.PopulateSampleRates(ACombo: TCombobox);
var
  i: Integer;
begin
  ACombo.Items.Clear;
  for i:=0 to High(SAMPLE_RATES) do
    ACombo.Items.Add(IntToStr(SAMPLE_RATES[i]));
  ACombo.ItemIndex := 0;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  rct: TRect;
  n: Integer;
begin
  ini := CreateIni;
  try
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    if L+W >= Screen.DesktopWidth then L := Screen.DesktopWidth - W;
    if L < 0 then L := 0;
    if T+H >= Screen.DesktopHeight then T := Screen.DesktopHeight - H;
    if T < 0 then T := 0;
    SetBounds(L, T, W, H);
    n := ini.ReadInteger('MainForm', 'WindowState', 0);
    if n = ord(wsMaximized) then
      WindowState := wsMaximized;

    CbSource.ItemIndex := ini.ReadInteger('MainForm', 'Source', CbSource.ItemIndex);
    CbSourceSelect(nil);
    PageControl.ActivePageIndex := ini.ReadInteger('MainForm', 'PageControl', PageControl.ActivePageIndex);
    PageControlChange(nil);
    InputPanel.Width := ini.ReadInteger('MainForm', 'InputPanelWidth', InputPanel.Width);

    NoDamageWarning_Mic := ini.ReadBool('Settings', 'NoWarning_SoundcardDamage', NoDamageWarning_Mic);
    NoDamageWarning_Out := ini.ReadBool('Settings', 'NoWarning_SpeakerDamage', NoDamageWarning_Out);
    SelectAudioEngine(ini.ReadString('Settings', 'AudioLib', CbAudioEngine.Text));

    FFrames[0].ControlPanel.Width := ini.ReadInteger('Oscilloscope', 'ControlPanelWidth', FFrames[0].ControlPanel.Width);
    ShowLinesAndSymbols := ini.ReadBool('Oscilloscope', 'ShowLinesAndSymbols', ShowLinesAndSymbols);

    FFrames[1].ControlPanel.Width := ini.ReadInteger('SpectrumAnalyzer', 'ControlPanelWidth', FFrames[1].ControlPanel.Width);
    LogFrequencyAxis := ini.ReadBool('SpectrumAnalyzer', 'LogFrequenyAxis', LogFrequencyAxis);
    SpectrumDB := ini.ReadBool('SpectrumAnalyzer', 'dB', SpectrumDB);

  finally
    ini.Free;
  end;
end;

procedure TMainForm.SelectAudioEngine(AudioEngineName: String);
var
  engine, s: String;
  i, idx: Integer;
begin
  if AudioEngineName = '' then
    exit;

  idx := -1;
  engine := Uppercase(AudioEngineName);
  for i := 0 to CbAudioEngine.Items.Count-1 do
    if Uppercase(CbAudioEngine.Items[i]) = engine then
    begin
      idx := i;
      break;
    end;
  if idx = -1 then
  begin
    MessageDlg('Audio engine "' + AudioEngineName +'" not implemented.', mtError, [mbOK], 0);
    exit;
  end;
  CbAudioEngine.ItemIndex := idx;

  if Assigned(DataCollector) then
  begin
    DataCollector.Close;
    DataCollector.Free;
  end;
  FGeneratorStream.Free;

  sleep(100);  // Why?

  if engine = 'BASS' then
  begin
    DataCollector := TBassDataCollector.Create(Handle);
    if not DataCollector.Open(44100, 2) then
      MessageDlg(DataCollector.ErrMsg, mtError, [mbOK], 0);
  end;

  if engine = 'UOS' then
  begin
    DataCollector := TuosDataCollector.Create(Handle);
    if not DataCollector.Open(44100, 2) then
      MessageDlg('Some libraries did not load', mtError, [mbOK], 0);
  end;

  FFrames[0].SetDataCollector(DataCollector);
  FFrames[1].SetDatacollector(DataCollector);

  FGeneratorStream := TMemoryStream.Create;
end;

procedure TMainForm.SwFrequencyChange(Sender: TObject);
begin
  if FFrequencyLock > 0 then
    exit;
  inc(FFrequencyLock);
  UpdateFrequency;
  UpdateWav;
  dec(FFrequencyLock);
end;

procedure TMainForm.TbDutyCycleChange(Sender: TObject);
begin
  if FDutyCycleLock > 0 then
    exit;
  inc(FDutyCycleLock);
  EdDutyCycle.Value := TbDutyCycle.Position;
  UpdateWav;
  dec(FDutyCycleLock);
end;

procedure TMainForm.TbRecordingLevelChange(Sender: TObject);
begin
  if not Assigned(DataCollector) then
    exit;
  DataCollector.SetRecordingInputActive(CbInputDevice.ItemIndex, true, TbRecordingLevel.Position / 100);
  InfoRecordingLevel.Caption := IntToStr(TbRecordingLevel.Position) + '%';
end;

procedure TMainForm.TbVolumeChange(Sender: TObject);
begin
  if FVolumeLock > 0 then
    exit;
  inc(FVolumeLock);
  EdVolume.Value := TbVolume.Position;
  UpdateWav;
  dec(FVolumeLock);
end;

procedure TMainForm.UpdateCaption;
begin
  case PageControl.ActivePageIndex of
    0: if CbSource.ItemIndex = 2 then
         Caption := 'Wave generator' else
         Caption := 'Oscilloscope';
    1: Caption := 'Spectrum analyzer';
  end;
end;

procedure TMainForm.UpdateFrequency;
begin
  if SwFrequency.Index < Length(FREQUENCIES) then
    EdFrequency.Value := PotFrequency.Position * FREQUENCIES[SwFrequency.Index] / 1000;
end;

procedure TMainForm.UpdateInputInfo;
var
  level: Single;
begin
  level := DataCollector.GetRecordingSensitivity(CbInputDevice.ItemIndex);
  TbRecordingLevel.Position := Round(level * 100);	// set the level slider
  InfoRecordingLevel.Caption := IntToStr(TbRecordingLevel.Position) + '%';
  LblDeviceType.Caption := DataCollector.GetInputType(CbInputDevice.ItemIndex);
end;

procedure TMainForm.UpdateWav;
var
  ch: Integer;
  wf: TWaveForm;
  wasRunning: Boolean;
begin
  if Assigned(DataCollector) and (DataCollector.Running or DataCollector.Paused)
  then begin
    wasRunning := DataCollector.Running;
    DataCollector.Stop;
    {
    if CbVolumeLeftON.Checked and not CbVolumeRightOn.Checked then ch := 1
      else if not cbVolumeLeftON.Checked and CbVolumeRightON.Checked then ch := 2
        else if CbVolumeLeftON.Checked and CbVolumeRightON.Checked then ch := 3
          else exit;
    }
    ch := 3;
    DataCollector.NumChannels := 1; //IfThen(ch = 1, 1, 2);
    if BtnSineWave.Down then wf := wfSine else
      if BtnTriangWave.Down then wf := wfTri else
        if BtnSquareWave.Down then wf := wfRect;
    FGeneratorStream.Clear;
    WriteWavStream(FGeneratorStream, EdFrequency.Value*1000, EdVolume.Value/100,
      10, SAMPLE_RATES[CbGeneratorSampleRate.ItemIndex], ch, wf, EdDutyCycle.Value/100);
    if wasRunning then
      FCurrentFrame.StartPlayback(FGeneratorStream.Memory, FGeneratorStream.Size, true);
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    if WindowState = wsNormal then
    begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
    end;
    ini.WriteInteger('MainForm', 'WindowState', ord(WindowState));
    ini.WriteInteger('MainForm', 'Source', CbSource.ItemIndex);
    ini.WriteInteger('MainForm', 'PageControl', PageControl.ActivePageIndex);
    ini.WriteInteger('MainForm', 'InputPanelWidth', InputPanel.Width);

    ini.WriteBool('Settings', 'NoWarning_SoundcardDamage', NoDamageWarning_Mic);
    ini.WriteBool('Settings', 'NoWarning_SpeakerDamage', NoDamageWarning_Out);
    ini.WriteString('Settings', 'AudioLib', CbAudioEngine.Text);

    ini.WriteInteger('Oscilloscope', 'ControlPanelWidth', FFrames[0].ControlPanel.Width);
    ini.WriteBool('Oscilloscope', 'ShowLinesAndSymbols', ShowLinesAndSymbols);

    ini.WriteInteger('SpectrumAnalyzer', 'ControlPanelWidth', FFrames[1].ControlPanel.Width);
    ini.WriteBool('SpectrumAnalyzer', 'LogFrequenyAxis', LogFrequencyAxis);
    ini.WriteBool('SpectrumAnalyzer', 'dB', SpectrumDB);
  finally
    ini.Free;
  end;
end;

end.

