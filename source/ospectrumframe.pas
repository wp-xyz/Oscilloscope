unit oSpectrumFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Types,
  Graphics, uESelector, ueled, TASources, TACustomSource, TACustomSeries,
  TAGraph, TASeries, TATransformations, TATools, oGlobal, oBaseFrame;

type

  { TSpectrumFrame }

  TSpectrumFrame = class(TBaseFrame)
    CbLogarithmic: TCheckBox;
    CbYdB: TCheckBox;
    FrequencyAxisTransform: TChartAxisTransformations;
    FrequencyAxisTransformLog: TLogarithmAxisTransform;
    GbInfo: TGroupBox;
    GbSamplingPeriod: TGroupBox;
    LogLabelsSource: TListChartSource;
    Panel2: TPanel;
    RgChannels: TRadioGroup;
    TxtInfo: TLabel;
    TxtSamplingPeriod: TLabel;
    RgSamples: TRadioGroup;
    procedure CbLogarithmicChange(Sender: TObject);
    procedure CbYdBChange(Sender: TObject);
    procedure DataPointCrosshairToolDraw(ASender: TDataPointDrawTool);
    procedure LeftChannelChartSourceGetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure RgChannelsClick(Sender: TObject);
    procedure RgSamplesClick(Sender: TObject);
    procedure RightChannelChartSourceGetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure TimerEventHandler(Sender: TObject);

  private
    FData: array of Single;
    FTimebaseLock: Integer;

  protected
    function GetNumChannels: Integer; inline;
    function GetNumSamples: Integer; inline;
    procedure PopulateLogLabels;
    procedure PopulateNumSamples;
    procedure SetSensitivity(AChannelIndex: TChannelIndex; AValue: Double); override;
    procedure SetupTimebase; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Activate; override;
    procedure Deactivate; override;
  end;


implementation

{$R *.lfm}

uses
  Math, StrUtils,
  TAChartUtils,ouosDataCollector, omain,
  oUtils;

constructor TSpectrumFrame.Create(AOwner: TComponent);
begin
  inherited;
  inc(FTimebaseLock);
  PopulateNumSamples;
  dec(FTimebaseLock);
  LogLabelsSource.Clear;
  PopulateLogLabels;
  CbLogarithmic.Checked := LogFrequencyAxis;
  CbLogarithmicChange(nil);
  TxtSamplingPeriod.Caption := '';
  BoldRadiogroup(RgSamples);
  BoldRadioGroup(RgChannels);
  PopulateSensitivity(SwLeftSensitivity);
  PopulateSensitivity(SwRightSensitivity);
end;

procedure TSpectrumFrame.Activate;
begin
  inherited;
  CbLogarithmic.Checked := LogFrequencyAxis;
  SetupTimebase;
  Timer.Enabled := FDataCollector.Running;
end;

procedure TSpectrumFrame.CbLogarithmicChange(Sender: TObject);
begin
  LogFrequencyAxis := CbLogarithmic.Checked;
  if LogFrequencyAxis then begin
    FrequencyAxisTransformLog.Enabled := true;
    if Chart.IsZoomed then
      with Chart.BottomAxis do
      begin
        Intervals.Options := Intervals.Options + [aipGraphcoords];
        Marks.Source := nil;
        Marks.Style := smsValue;
      end
    else
      with Chart.BottomAxis do
      begin
        Chart.BottomAxis.Marks.Source := LogLabelsSource;
        Chart.BottomAXis.Marks.Style := smsLabel;
      end;
  end else
  begin
    FrequencyAxisTransformLog.Enabled := false;
    with Chart.BottomAxis do
    begin
      Intervals.Options := Intervals.Options - [aipGraphcoords];
      Marks.Source := nil;
      Marks.Style := smsValue;
    end;
  end;
end;

procedure TSpectrumFrame.CbYdBChange(Sender: TObject);
begin
  ParamsToControls;
end;

procedure TSpectrumFrame.DataPointCrosshairToolDraw(ASender: TDataPointDrawTool);
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
    'f = %.3f kHz'#13 +
    '%s = %.3f', [
    series.GetXValue(ASender.PointIndex), s, series.GetYValue(ASender.PointIndex)
  ]);
  GbInfo.Show;
end;

procedure TSpectrumFrame.Deactivate;
begin
  Timer.Enabled := false;
  inherited;
end;

function TSpectrumFrame.GetNumChannels: Integer;
begin
  Result := succ(RgChannels.ItemIndex);
end;

function TSpectrumFrame.GetNumSamples: Integer;
begin
  Result := NUM_SAMPLES[RgSamples.ItemIndex];
end;

procedure TSpectrumFrame.LeftChannelChartSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
{ from bass.chm:
  "With a 2048 sample FFT, there will be 1024 floating-point values returned...
  Each value, or "bin", ranges from 0 to 1 (can actually go higher if the
  sample data is floating-point and not clipped). The 1st bin contains the
  DC component, the 2nd contains the amplitude at 1/2048 of the
  channel's sample rate, followed by the amplitude at 2/2048, 3/2048, etc. }
begin
  Unused(ASource);
  // in case of an individual FFT of both channels the data array contains
  // interleaved left and right channel values.
  // For stereo, the even indexes are for the left channel
  // NOTE: For sampling of stereo data, the effective sample rate is halved!
  AIndex := AIndex * GetNumChannels;
  AItem.X := AIndex / GetNumSamples * FSampleRate * 0.001;
  if CbYdB.Checked then begin
    if FData[AIndex] = 0.0 then AItem.Y := NaN else
      AItem.Y := 10*Log10(FData[AIndex])
  end else
    AItem.Y := FData[AIndex] * 100;
end;

procedure TSpectrumFrame.PopulateLogLabels;
var
  mantisse, exponent: Integer;
  decade, value: Double;
  str: String;
begin
  LogLabelsSource.Clear;
  for exponent := -4 to 4 do
  begin
    decade := IntPower(10.0, exponent);
    for mantisse := 1 to 9 do begin
      value := mantisse * decade;
      if mantisse in [5, 7, 9] then
        str := ''
      else
        str := Format('%.4g', [value]);
      LogLabelsSource.Add(value, value, str);
    end;
  end;
end;

procedure TSpectrumFrame.PopulateNumSamples;
var
  i: Integer;
begin
  RgSamples.Items.Clear;
  for i:=0 to High(NUM_SAMPLES) do
    RgSamples.Items.Add(IntToStr(NUM_SAMPLES[i]));
  RgSamples.ItemIndex := RgSamples.Items.IndexOf('2048');
end;

procedure TSpectrumFrame.RgChannelsClick(Sender: TObject);
begin
  SetupTimebase;
end;

procedure TSpectrumFrame.RgSamplesClick(Sender: TObject);
begin
  SetupTimebase;
end;

procedure TSpectrumFrame.RightChannelChartSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  Unused(ASource);
  if GetNumSamples = 2 then
    AIndex := AIndex * 2 + 1;
  AItem.X := AIndex / GetNumSamples * FSampleRate * 0.001;
  if CbYdB.Checked then begin
    if FData[AIndex] = 0.0 then AItem.Y := NaN else
      AItem.Y := 10*Log10(FData[AIndex])
  end else
    AItem.Y := FData[AIndex] * 100;
end;

procedure TSpectrumFrame.SetSensitivity(AChannelIndex: TChannelIndex;
  AValue: Double);
begin
  with GetAxis(AChannelIndex) do
    if CbYdB.Checked then
    begin
      Range.Max := 0;
      Range.Min := -80;
      Range.UseMax := true;
      Range.UseMin := true;
      Title.Caption := IfThen(AChannelIndex=ciLeft, 'Left', 'Right')
        + 'channel spectral power (dB)';
    end else
    begin
      Range.Max := AValue;
      Range.Min := 0;
      Range.UseMax := true;
      Range.UseMin := true;
      Title.Caption := IfThen(AChannelIndex=ciLeft, 'Left', 'Right')
        + ' channel spectral power (% of full scale)';
    end;
end;

procedure TSpectrumFrame.SetupTimebase;
var
  bufsize, n: Integer;
  t: double;
  ch: Integer;
begin
  if FTimebaseLock > 0 then
    exit;

  if FSampleRate = 0 then
  begin
    if FDataCollector.Running then
      FSampleRate := FDataCollector.SampleRate
    else
      exit;
  end;

  ch := GetNumChannels;
  n := GetNumSamples;                // sample rate = n / t, samples per second
  t := n / FSampleRate * 1000;       // *1000 to convert sec to ms

  FDataCollector.SampleRate := FSampleRate;
  TxtSamplingPeriod.Caption := Format('%.0fms', [t]);
  bufsize := n div 2 * ch * SizeOf(Single);
  SetLength(FData, bufsize);
  LeftChannelChartSource.PointsNumber := n div (2*ch);
  RightChannelChartSource.PointsNumber := n div (2*ch);

  Timer.Interval := ceil(t);
end;

procedure TSpectrumFrame.TimerEventHandler(Sender: TObject);
var
  n: Integer;
begin
  // Stop if no more data available
  if not FDataCollector.Running then
  begin
    Stop;
    exit;
  end;

  // Get data
  n := FDataCollector.GetFFTData(@FData[0], GetNumSamples, GetNumChannels);
  
 if MainForm.CbAudioEngine.text = 'uos' then FData := BData;

  // prepare series
  LeftChannelChartSource.Reset;
  RightChannelChartSource.Reset;

  // Repaint chart
  Chart.Invalidate;

  // Progress info display
  if not FCrosshairActive then begin
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

