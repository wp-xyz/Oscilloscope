unit oOscilloscopeFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, ExtCtrls, StdCtrls, Spin, Buttons,
  TAGraph, TASeries, TACustomSource, TASources, TATransformations, TATools,
  uESelector, uEKnob,
  oGlobal, oBaseFrame;

type

  { TOscilloscopeFrame }

  TOscilloscopeFrame = class(TBaseFrame)
    Bevel1: TBevel;
    BtnTriggerAsc: TSpeedButton;
    BtnTriggerDesc: TSpeedButton;
    BtnTriggerLeft: TSpeedButton;
    BtnTriggerRight: TSpeedButton;
    CbShowSeriesPoints: TCheckBox;
    EdTriggerLevel: TFloatSpinEdit;
    GbInfo: TGroupBox;
    GbTimeBase: TGroupBox;
    GbTrigger: TGroupBox;
    Splitter1: TSplitter;
    TriggerPanel: TPanel;
    TimeBasePanel: TPanel;
    Panel2: TPanel;
    SwTimebase: TuESelector;
    PotTriggerLevel: TuEKnob;
    TxtFrequency: TLabel;
    TxtInfo: TLabel;
    procedure CbShowSeriesPointsChange(Sender: TObject);
    procedure DataPointCrosshairToolDraw(ASender: TDataPointDrawTool);
    procedure EdTriggerLevelChange(Sender: TObject);
    procedure LeftChannelChartSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure PotTriggerLevelChange(Sender: TObject);
    procedure RightChannelChartSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure SwTimebaseChange(Sender: TObject);
    procedure TimerEventHandler(Sender: TObject);

  private
    FTriggerLevelLock: Integer;
    FTriggerIndex: Integer;
    FTriggerTime: Double;
    FData: TChannelDataArray;
    // ****** NEW ***************
    procedure DataAvailHandler(ABufPtr: Pointer; ABufSize: Integer);
    // ****************************

  protected
    function CalcFrequency(AChannelIndex: TChannelIndex): Double;
    procedure FindTriggerIndex(AChannelIndex: TChannelIndex; ALevel: Double);
    function IndexToMilliseconds(AIndex: Double): Double;
    procedure PopulateTimebase;
    procedure SetSensitivity(AChannelIndex: TChannelIndex; AValue: Double); override;
    procedure SetupTimebase; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure AutoSizeControls; override;
    // *********** NEW *************
    procedure Prepare; override;
    // *****************************
  end;

implementation

{$R *.lfm}

uses
  Math,
  TAChartUtils, TAChartAxis, TACustomSeries,
  oUtils, oDataCollector;


{ TOscilloscopeFrame }

constructor TOscilloscopeFrame.Create(AOwner: TComponent);
begin
  inherited;
  PopulateTimebase;
  PopulateSensitivity(SwLeftSensitivity);
  PopulateSensitivity(SwRightSensitivity);
  TxtFrequency.Caption := '';
  CbShowSeriesPoints.Checked := ShowLinesAndSymbols;
end;

procedure TOscilloscopeFrame.Activate;
begin
  inherited;
  CbShowSeriesPoints.Checked := ShowLinesAndSymbols;
  SetupTimebase;
  Timer.Enabled := Assigned(FDataCollector) and FDataCollector.Running;
end;

procedure TOscilloscopeFrame.AutoSizeControls;
var
  w: Integer;
  cnv: TControlCanvas;
begin
  if not HandleAllocated then exit;

  cnv := TControlCanvas.Create;
  try
    cnv.Control := GbTimeBase;
    cnv.Font.Assign(GbTimeBase.Font);
    w := cnv.TextWidth(GbTimeBase.Caption);
  finally
    cnv.Free;
  end;

  ControlPanel.Constraints.MinWidth := (w + Scale96ToFont(32)) * 2 + CenterBevel.Width;
end;

function TOscilloscopeFrame.CalcFrequency(AChannelIndex: TChannelIndex): Double;
var
  n, i: Integer;
  i1, i2: Integer;
  t1, t2: Double;
  y1, y2: SmallInt;
begin
  if Length(FData) = 0 then
  begin
    Result := NaN;
    exit;
  end;

  n := 0;
  i := 1;
  i1 := 0;
  while i < Length(FData) do
  begin
    if (FData[i-1, AChannelIndex] <= 0) and (FData[i, AChannelIndex] > 0) then
    begin
      i1 := i;
      inc(i);
      break;
    end;
    inc(i);
  end;
  i2 := i1;
  while i < Length(FData) do
  begin
    if (FData[i-1, AChannelIndex] <= 0) and (FData[i, AChannelIndex] > 0) then
    begin
      i2 := i;
      inc(n);
    end;
    inc(i);
  end;
  if i2 <> i1 then begin
    y1 := FData[i1-1, AChannelIndex];
    y2 := FData[i1, AChannelIndex];
    t1 := IndexToMilliseconds(i1 - 1 - y1/(y2-y1));

    y1 := FData[i2-1, AChannelIndex];
    y2 := FData[i2, AChannelIndex];
    t2 := IndexToMilliseconds(i2 - 1 - y1/(y2-y1));

    Result := 1000.0 * n / (t2 - t1);
  end else
    Result := NaN;
end;

procedure TOscilloscopeFrame.CbShowSeriesPointsChange(Sender: TObject);
begin
  ShowLinesAndSymbols := CbShowSeriesPoints.Checked;
  LeftChannelSeries.ShowPoints := ShowLinesAndSymbols;
  RightChannelSeries.ShowPoints := ShowLinesAndSymbols;
end;

// ******************* NEW ***************
{ This event handler is called when the sound system has new data in its buffer.
  ABufPtr points to the data received, ABufSize is the number of bytes in the buffer.
  This routine must copy these data into the frame's buffer and plot the chart,
  essentiall the same that is in TimerEventHandler. }
procedure TOscilloscopeFrame.DataAvailHandler(ABufPtr: Pointer; ABufSize: Integer);
var
  numDataPerChannel: Integer;
  f: Double;
  level: Double;
  triggerch: TChannelIndex;
  s: String;
begin
  // In our case the data are 16bit integers, in case of stereo alternating
  // between left and right channel,
  numDataPerChannel := ABufSize div SizeOf(SmallInt) div FDataCollector.NumChannels;

  // Copy data to FData
  SetLength(FData, numDataPerChannel);
  Move(ABufPtr^, FData[0], ABufSize);

  // prepare series
  LeftChannelChartSource.PointsNumber := numDataPerChannel;
  LeftChannelChartSource.Reset;
  RightChannelChartSource.PointsNumber := numDataPerChannel;
  RightChannelChartSource.Reset;

  // Find trigger time
  if BtnTriggerAsc.Down or BtnTriggerDesc.Down then
  begin
    level := EdTriggerLevel.value * 0.01 * $7FFF;
    if BtnTriggerLeft.Down then triggerch := ciLeft else triggerch := ciRight;
  end;
  FindTriggerIndex(triggerCh, level);

  // Repaint chart
  Chart.Invalidate;

  // Frequency display
  s := 'Freq ';
  f := CalcFrequency(ciLeft);
  if IsNaN(f) then s := s + 'L: -; ' else s := s + Format('L: %.3fHz ', [f]);
  f := CalcFrequency(ciRight);
  if IsNaN(f) then s := s + lineending + 'Freq R: -; ' else s := s + lineending + Format('Freq R: %.3fHz', [f]);
  TxtFrequency.Caption := s;
  TxtFrequency.Repaint;

  // Progress info display
  if not FCrosshairActive then
  begin
    TxtInfo.Caption := Format(
      'Time: %s'#13+
      'Bytes: %.0n', [
      FormatDateTime('nn:ss.zzz', FDataCollector.GetRunningTime / (24*60*60)),
      FDataCollector.GetPlayedBytes*1.0
    ]);
    if not GbInfo.Visible then GbInfo.Show;
  end;

  // Notify main form of received data
  if Assigned(OnDataReceived) then
    OnDataReceived(self);
end;
// ***************************************

procedure TOscilloscopeFrame.DataPointCrosshairToolDraw(
  ASender: TDataPointDrawTool);
var
  series: TChartSeries;
  s: String;
begin
  series := TChartSeries(ASender.Series);
  if series = LeftChannelSeries then
    s := 'L'
  else if series = RightChannelSeries then
    s := 'R'
  else
    exit;
  TxtInfo.Caption := Format(
    't = %.1f ms'#13 +
    '%s = %.3f %%', [
    series.GetXValue(ASender.PointIndex), s, series.GetYValue(ASender.PointIndex)
  ]);
  GbInfo.Show;
end;

procedure TOscilloscopeFrame.Deactivate;
begin
  Timer.Enabled := false;
  inherited;
end;

procedure TOscilloscopeFrame.EdTriggerLevelChange(Sender: TObject);
begin
  if FTriggerLevelLock > 0 then
    exit;
  inc(FTriggerLevelLock);
  PotTriggerLevel.Position := EdTriggerLevel.Value;
  dec(FTriggerLevelLock);
end;

procedure TOscilloscopeFrame.FindTriggerIndex(AChannelIndex: TChannelIndex;
  ALevel: Double);
var
  i: Integer;
  y1,y2: Double;
  delta: Double;
begin
  FTriggerIndex := 0;
  FTriggerTime := 0.0;
  if BtnTriggerAsc.Down or BtnTriggerDesc.Down then
  begin
    i := 0;
    if BtnTriggerAsc.Down then  // trigger +
    begin
      while (i < Length(FData)) and (FData[i, AChannelIndex] > ALevel) do
        inc(i);
      while (i < Length(FData)) and (FData[i, AChannelIndex] < ALevel) do
        inc(i)
    end else
    if BtnTriggerDesc.Down then  // trigger -
    begin
      while (i < Length(FData)) and (FData[i, AChannelIndex] < ALevel) do
        inc(i);
      while (i < Length(FData)) and (FData[i, AChannelIndex] > ALevel) do
        inc(i);
    end;
    FTriggerIndex := i;
    if i >= Length(FData) then
      exit;
    if FData[i, AChannelIndex] = ALevel then
      FTriggerTime := IndexToMilliseconds(i)
    else
    begin
      y1 := FData[i-1, AChannelIndex] - ALevel;
      y2 := FData[i, AChannelIndex] - ALevel;
      if (y1 <> y2) then
        FTriggerTime := IndexToMilliseconds(i - 1 - y1 / (y2 - y1)) else
        FTriggerTime := 0.0;
    end;
  end;
end;

function TOscilloscopeFrame.IndexToMilliseconds(AIndex: Double): Double;
begin
  Result := AIndex * 1000.0 / FSampleRate * FDataCollector.NumChannels;
end;

procedure TOscilloscopeFrame.LeftChannelChartSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  Unused(ASource);
  AItem.X := IndexToMilliseconds(AIndex) - FTriggerTime;   // convert index to ms
  if AItem.X < 0 then
  begin
    AItem.X := 0.0;
    if BtnTriggerLeft.Down then
      AItem.Y := PotTriggerLevel.Position else
      AItem.Y := NaN;
  end else
    AItem.Y := FData[AIndex, ciLeft] / WAVE_MAX * 100;
end;

procedure TOscilloscopeFrame.PopulateTimebase;
var
  L: TStrings;
  i: Integer;
begin
  L := TStringList.Create;
  try
    for i:=0 to High(MS_PER_DIV) do
      L.Add(Format('%.1g', [MS_PER_DIV[i]]));
    SwTimebase.Items.Assign(L);
    SwTimebase.Index := L.IndexOf('1');
  finally
    L.Free;
  end;
end;

procedure TOscilloscopeFrame.PotTriggerLevelChange(Sender: TObject);
begin
  if FTriggerLevelLock > 0 then
    exit;
  inc(FTriggerLevelLock);
  EdTriggerLevel.Value := PotTriggerLevel.Position;
  dec(FTriggerLevelLock);
end;

//**************** NEW *********************
procedure TOscilloscopeFrame.Prepare;
begin
  FDataCollector.OnDataAvail := @DataAvailHandler;
end;
//*******************************************
procedure TOscilloscopeFrame.RightChannelChartSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  Unused(ASource);
  AItem.X := IndexToMilliseconds(AIndex) - FTriggerTime;   // convert index to ms
  if (AItem.X < 0) then
  begin
    AItem.X := 0.0;
    if BtnTriggerRight.Down then
      AItem.Y := PotTriggerLevel.Position else
      AItem.Y := NaN;
  end else
    AItem.Y := FData[AIndex, ciRight] / WAVE_MAX * 100;
end;

procedure TOscilloscopeFrame.SetSensitivity(AChannelIndex: TChannelIndex;
  AValue: Double);
begin
  with GetAxis(AChannelIndex).Range do
  begin
    Max := AValue;
    Min := -AValue;
    UseMax := true;
    UseMin := true;
  end;
end;

procedure TOscilloscopeFrame.SetupTimebase;
var
  n: Integer;           // number of samples
  period: Double;       // time span, in ms, to be captured
begin
  if FSampleRate = 0 then
  begin
    if Assigned(FDataCollector) and FDataCollector.Running then
      FSampleRate := FDataCollector.SampleRate
    else
      exit;
  end;

  Chart.Extent.XMin := 0;
  Chart.Extent.XMax := Chart.BottomAxis.Intervals.Count * MS_PER_DIV[SwTimebase.Index];
  Chart.Extent.UseXMax := true;
  Chart.Extent.UseXMin := true;

  // Time period, in ms, to be captured
  period := Chart.Extent.XMax - Chart.Extent.XMin;

  // Calc number of samples needed for full screen, plus - say - 50% spare for
  // trigger
  n := round(1.5 * period / 1000 * FSampleRate);
  if n < 2048 then n := 2048;
  SetLength(FData, n);
  LeftChannelChartSource.PointsNumber := n;
  RightChannelChartSource.PointsNumber := n;

  Timer.Interval := Round(1000.0 * n / FSampleRate);
end;

procedure TOscilloscopeFrame.SwTimebaseChange(Sender: TObject);
begin
  SetupTimebase;
end;

procedure TOscilloscopeFrame.TimerEventHandler(Sender: TObject);
var
  n, i: Integer;
  level: Double;
  triggerch: TChannelIndex;
  f: Double;
  s: String;
begin
  // Stop if no more data available

  if not FDataCollector.Running then
  begin
    Stop;
    exit;
  end;

  if Length(FData) = 0 then  exit;
    
  // Get data
  n := FDataCollector.GetWaveData(@FData[0], Length(FData)*SizeOf(TChannelData));
  
   // prepare series
  LeftChannelChartSource.Reset;
  RightChannelChartSource.Reset;

  // Find trigger time
  if BtnTriggerAsc.Down or BtnTriggerDesc.Down then
  begin
    level := EdTriggerLevel.value * 0.01 * $7FFF;
    if BtnTriggerLeft.Down then triggerch := ciLeft else triggerch := ciRight;
  end;
  FindTriggerIndex(triggerCh, level);

  // Repaint chart
  Chart.Invalidate;

  // Frequency display
  s := 'Freq ';
  f := CalcFrequency(ciLeft);
  if IsNaN(f) then s := s + 'L: -; ' else s := s + Format('L: %.3fHz ', [f]);
  f := CalcFrequency(ciRight);
  if IsNaN(f) then s := s + lineending + 'Freq R: -; ' else s := s +
  lineending + Format('Freq R: %.3fHz', [f]);
  TxtFrequency.Caption := s;
  TxtFrequency.Repaint;

  // Progress info display
  if not FCrosshairActive then
  begin
    TxtInfo.Caption := Format(
      'Time: %s'#13+
      'Bytes: %.0n', [
      FormatDateTime('nn:ss.zzz', FDataCollector.GetRunningTime / (24*60*60)),
      FDataCollector.GetPlayedBytes*1.0
    ]);
    if not GbInfo.Visible then GbInfo.Show;
  end;

  // Notify main form of received data
  if Assigned(OnDataReceived) then
    OnDataReceived(self);
end;

end.

