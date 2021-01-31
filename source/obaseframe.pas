unit oBaseFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  TAGraph, TASeries, TASources, TATransformations, TAChartAxis, TATools,
  Forms, ExtCtrls, StdCtrls, ueSelector, ueLED,
  oGlobal, oDataCollector, types;

type
  TDataAvailEvent = procedure(Sender: TObject; AWave: TWaveDataArray) of object;

  { TBaseFrame }
  TBaseFrame = class(TFrame)
    CbLinkedSensitivities: TCheckBox;
    CenterBevel: TBevel;
    CbLeftON: TCheckBox;
    CbRightON: TCheckBox;
    Chart: TChart;
    ChartToolset: TChartToolset;
    ControlPanel: TPanel;
    DataPointCrosshairTool: TDataPointCrosshairTool;
    GbLeftSensitivity: TGroupBox;
    GbRightSensitivity: TGroupBox;
    LEDLeft: TueLED;
    LEDRightPanel: TPanel;
    LEDRight: TueLED;
    LeftAxisTransformations: TChartAxisTransformations;
    LeftAxisTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    LeftChannelChartSource: TUserDefinedChartSource;
    LeftChannelSeries: TLineSeries;
    PanDragTool: TPanDragTool;
    Panel1: TPanel;
    LeftSensitityPanel: TPanel;
    LEDLeftPanel: TPanel;
    RightSensitivityPanel: TPanel;
    RightAxisTransformations: TChartAxisTransformations;
    RightAxisTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    RightChannelChartSource: TUserDefinedChartSource;
    RightChannelSeries: TLineSeries;
    SwLeftSensitivity: TuESelector;
    SwRightSensitivity: TuESelector;
    Timer: TTimer;
    ZoomDragTool: TZoomDragTool;
    procedure CbLeftONClick(Sender: TObject);
    procedure CbRightONClick(Sender: TObject);
    procedure DataPointCrosshairToolAfterMouseDown(ATool: TChartTool;
      APoint: TPoint);
    procedure DataPointCrosshairToolAfterMouseUp(ATool: TChartTool;
      APoint: TPoint);
    procedure SwLeftSensitivityChange(Sender: TObject);
    procedure SwRightSensitivityChange(Sender: TObject);

  private
    FMode: TPlayRecordMode;
    FOnBeginPlayback: TNotifyEvent;
    FOnEndPlayback: TNotifyEvent;
    FOnDataAvail: TDataAvailEvent;
    FOnDataReceived: TNotifyEvent;
    procedure SetSensValue(AChannelIndex: TChannelIndex; AValue: Double);

  protected
    FDataCollector: TDataCollector;
    FSampleRate: Integer;
    FSensitivityLock: Integer;
    FCrosshairActive: Boolean;
    function LeftAxis: TChartAxis; inline;
    function RightAxis: TChartAxis; inline;
    function GetAxis(AChannelindex: TChannelIndex): TChartAxis;
    procedure SetSensitivity(AChannelIndex: TChannelIndex; AValue: Double); virtual; abstract;
    procedure SetupTimebase; virtual; abstract;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Activate; virtual;
    procedure Deactivate; virtual;

    procedure AutoSizeControls; virtual;

    procedure SetDataCollector(AValue: TDataCollector);
    procedure SetSampleRate(AValue: Integer);

    function StartPlayback(const AFileName: String): Boolean; overload;
    function StartPlayback(AMemory: Pointer; ALength: DWord; ALoop: Boolean): Boolean; overload;
    function StartRecording(ASampleRate: Integer): Boolean;
    procedure Stop;

    procedure Pause;
    procedure Resume;

    procedure ControlsToParams;
    procedure ParamsToControls;

    property OnBeginPlayback: TNotifyEvent
      read FOnBeginPlayback write FOnBeginPlayback;
    property OnEndPlayback: TNotifyEvent
      read FOnEndPlayback write FOnEndPlayback;
    property OnDataReceived: TNotifyEvent
      read FOnDataReceived write FOnDataReceived;
  end;


implementation

{$R *.lfm}

uses
  Math, Dialogs,
  TAChartUtils, TAChartAxisUtils;

constructor TBaseFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TBaseFrame.Destroy;
begin
  inherited;
end;

procedure TBaseFrame.Activate;
begin
  ParamsToControls;
end;

procedure TBaseFrame.AutoSizeControls;
begin
  // to be overridden by ancestors if needed
end;

procedure TBaseFrame.CbLeftONClick(Sender: TObject);
begin
  LEDLeft.Active := CbLeftON.Checked;
  LeftChannelSeries.Active := CbLeftON.Checked;
  with LeftAxis do
  begin
    Visible := CbLeftON.Checked;
    Range.UseMin := CbLeftON.Checked;
    Range.UseMax := CbLeftON.Checked;
  end;
  with RightAxis do
  begin
    Alignment := TChartAxisAlignment(IfThen(LeftAxis.Visible, ord(calRight), ord(calLeft)));
    Title.LabelFont.Orientation := IfThen(Alignment = calRight, -900, 900);
  end;
end;

procedure TBaseFrame.CbRightONClick(Sender: TObject);
begin
  LEDRight.Active := CbRightON.Checked;
  RightChannelSeries.Active := CbRightON.Checked;
  with RightAxis do
  begin
    Visible := CbRightON.Checked;
    Range.UseMin := CbRightON.Checked;
    Range.UseMax := CbRightON.Checked;
    Alignment := TChartAxisAlignment(IfThen(LeftAxis.Visible, ord(calRight), ord(calLeft)));
    Title.LabelFont.Orientation := IfThen(Alignment = calRight, -900, 900);
  end;
end;

procedure TBaseFrame.ControlsToParams;
begin
  SensitivityParams.ChannelMax[ciLeft] := SENSITIVITIES[SwLeftSensitivity.Index];
  SensitivityParams.ChannelMax[ciRight] := SENSITIVITIES[SwRightSensitivity.Index];

  SensitivityParams.ChannelOn[ciLeft] := CbLeftON.Checked;
  SensitivityParams.ChannelOn[ciRight] := CbRightON.Checked;

  SensitivityParams.ChannelsLinked := CbLinkedSensitivities.Checked;
end;

procedure TBaseFrame.DataPointCrosshairToolAfterMouseDown(ATool: TChartTool;
  APoint: TPoint);
begin
  Unused(ATool, APoint);
  FCrosshairActive := true;
end;

procedure TBaseFrame.DataPointCrosshairToolAfterMouseUp(ATool: TChartTool;
  APoint: TPoint);
begin
  Unused(APoint);
  if ATool is TDataPointCrosshairTool then
  begin
    TDatapointCrosshairTool(ATool).Hide;
    FCrosshairActive := false;
  end;
end;

procedure TBaseFrame.Deactivate;
begin
  ControlsToParams;
end;

function TBaseFrame.GetAxis(AChannelIndex: TChannelIndex): TChartAxis;
begin
  case AChannelindex of
    ciLeft : Result := LeftAxis;
    ciRight: Result := RightAxis;
  end;
end;

function TBaseFrame.LeftAxis: TChartAxis;
begin
  Result := Chart.AxisList[1];
end;

procedure TBaseFrame.ParamsToControls;
begin
  SetSensValue(ciLeft, SensitivityParams.ChannelMax[ciLeft]);
  SetSensvalue(ciRight, SensitivityParams.ChannelMax[ciRight]);

  CbLeftON.Checked := SensitivityParams.ChannelOn[ciLeft];
  CbLeftONClick(nil);

  CbRightON.Checked := SensitivityParams.ChannelOn[ciRight];
  CbRightOnClick(nil);

  CbLinkedSensitivities.Checked := SensitivityParams.ChannelsLinked;
end;

procedure TBaseFrame.Pause;
begin
  if FDataCollector.Running then
  begin
    Timer.Enabled := false;
    FDataCollector.Pause;
  end;
end;

procedure TBaseFrame.Resume;
begin
  if FDataCollector.Paused then
  begin
    Timer.Enabled := true;
    FDataCollector.Resume;
  end;
end;

function TBaseFrame.RightAxis: TChartAxis;
begin
  Result := Chart.AxisList[2];
end;

procedure TBaseFrame.SetDataCollector(AValue: TDataCollector);
begin
  FDataCollector := AValue;
end;

procedure TBaseFrame.SetSampleRate(AValue: Integer);
begin
  FSampleRate := AValue;
end;

procedure TBaseFrame.SetSensValue(AChannelIndex: TChannelIndex;
  AValue: Double);
const
  EPS = 1e-6;
var
  i: Integer;
begin
  // Set axis limits
  SetSensitivity(AChannelIndex, AValue);

  // Set rotary switch to corresponding value
  for i:= 0 to High(SENSITIVITIES) do
    if SameValue(SENSITIVITIES[i], AValue, EPS) then
    begin
      case AChannelIndex of
        ciLeft : SwLeftSensitivity.Index := i;
        ciRight: SwRightSensitivity.Index := i;
      end;
      exit;
    end;
end;

function TBaseFrame.StartPlayback(const AFileName: String): Boolean;
begin
  Result := false;

  if FDataCollector.Running then
    exit;

  if (AFilename = '') then
    exit;

  if not FileExists(AFileName) then
  begin
    MessageDlg(Format('File "%s" not found.', [AFileName]), mtError, [mbOK], 0);
    exit;
  end;

  if FDataCollector.StartPlayback(AFileName) then
  begin
    
   FSampleRate := FDataCollector.GetSampleRate;
   if Assigned(FOnBeginPlayback) then
      FOnBeginPlayback(self);
    SetupTimebase;
    FMode := prmPlay;
    Timer.Enabled := true;
    Result := true;
  end else begin
    Timer.Enabled := false;
    FMode := prmNone;
    MessageDlg(FDataCollector.ErrMsg, mtError, [mbOK], 0);
  end;
end;

function TBaseFrame.StartPlayback(AMemory: Pointer; ALength: DWord;
  ALoop: Boolean): Boolean;
begin
  Result := false;

  if FDataCollector.Running then
    exit;

  if (AMemory = nil) then
    exit;

  if FDataCollector.StartPlayback(AMemory, ALength, ALoop) then
  begin
    FSampleRate := FDataCollector.GetSampleRate;
    if Assigned(FOnBeginPlayback) then
      FOnBeginPlayback(self);
    SetupTimebase;
    FMode := prmPlay;
    Timer.Enabled := true;
    Result := true;
  end else begin
    Timer.Enabled := false;
    FMode := prmNone;
    MessageDlg(FDataCollector.ErrMsg, mtError, [mbOK], 0);
  end;
end;

function TBaseFrame.StartRecording(ASampleRate: Integer): Boolean;
begin
  Result := false;

  if FDataCollector.Running then
    exit;

  if FDataCollector.StartRecording(ASampleRate) then begin
    FSampleRate := ASampleRate;
    {
    if Assigned(FOnBeginRecording) then
      FOnBeginRecording(self);
      }
    SetupTimebase;
    FMode := prmRecord;
    Timer.Enabled := true;
    Result := true;
  end else
  begin
    Timer.Enabled := false;
    FMode := prmNone;
    MessageDlg(FDataCollector.ErrMsg, mtError, [mbOk], 0);
  end;
end;

procedure TBaseFrame.Stop;
begin
  FMode := prmNone;
  Timer.Enabled := false;
  FDataCollector.Stop;
  if Assigned(FOnEndPlayback) then
    FOnEndPlayback(self);
end;

procedure TBaseFrame.SwLeftSensitivityChange(Sender: TObject);
begin
  if FSensitivityLock > 0 then
    exit;
  inc(FSensitivityLock);
  try
    SetSensitivity(ciLeft, SENSITIVITIES[SwLeftSensitivity.Index]);
    if CbLinkedSensitivities.Checked then
    begin
      SwRightSensitivity.Index := SwLeftSensitivity.Index;
      SetSensitivity(ciRight, SENSITIVITIES[SwLeftSensitivity.Index]);
    end;
  finally
    dec(FSensitivityLock);
  end;
end;

procedure TBaseFrame.SwRightSensitivityChange(Sender: TObject);
begin
  if FSensitivityLock > 0 then
    exit;
  inc(FSensitivityLock);
  try
    SetSensitivity(ciRight, SENSITIVITIES[SwRightSensitivity.Index]);
    if CbLinkedSensitivities.Checked then
    begin
      SwLeftSensitivity.Index := SwRightSensitivity.Index;
      SetSensitivity(ciLeft, SENSITIVITIES[SwRightSensitivity.Index]);
    end;
  finally
    dec(FSensitivityLock);
  end;
end;

end.

