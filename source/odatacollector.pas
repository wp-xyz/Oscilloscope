unit oDataCollector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType,
  oGlobal;

type
  // ****************** NEW *****************
  TDataAvailEvent = procedure (ABufPtr: Pointer; ABufSize: Integer) of object;
  // ****************************************

  TDataCollector = class
  private
    FHandle: HWnd;
    FErrMsg: String;
    // *********** NEW *************
    FOnDataAvail: TDataAvailEvent;
    // *****************************
    function GetErrMsg: String;

  protected
    FWaveData: TWaveDataArray;
    FSampleRate: Integer;
    FNeedTimer: Boolean;
    FNumChannels: Integer;
    // *************** NEW *****************
    procedure DataAvailProc; virtual; abstract;
    // *************************************
    procedure SetError(const AMsg: String);
    procedure SetSampleRate(AValue: Integer); virtual;
    procedure SetNumChannels(AValue: Integer); virtual;

  public
    constructor Create(AHandle: HWnd); virtual;

    function Open(ASampleRate, ANumChannels: Integer): boolean; virtual; abstract;
    procedure Close; virtual; abstract;

    function HasError: Boolean;

    function StartPlayback(const AFileName: String): Boolean; virtual; overload; abstract;
    function StartPlayback(AMemory: Pointer; ALength: DWord;
      ALoop: Boolean): Boolean; virtual; overload; abstract;
    function StartRecording(ASampleRate: Integer): Boolean; virtual; abstract;
    procedure Stop; virtual; abstract;
    function Running: Boolean; virtual; abstract;

    procedure Pause; virtual; abstract;
    procedure Resume; virtual; abstract;
    function Paused: Boolean; virtual; abstract;

    function GetInputType(AIndex: Integer): String; virtual; abstract;
    procedure GetRecordingDeviceList(AList: TStrings; out AIndex: Integer); virtual; abstract;
    function GetRecordingInputOn(AIndex: Integer): boolean; virtual; abstract;
    function GetRecordingSensitivity(AIndex: Integer): Single; virtual; abstract;
    procedure SetRecordingInputActive(AIndex: Integer; Activate: Boolean;
      ASensitivity: Single = -1); virtual; abstract;

    function GetPlayedBytes: Int64; virtual; abstract;
    function GetRunningTime: Double; virtual; abstract;
    function GetSampleRate: Integer; virtual;

    function GetFFTData(ABufPtr: Pointer; ANumPoints, ANumChannels: Integer): Integer; virtual; abstract;
    function GetWaveData(ABufPtr: Pointer; ABufSize: Integer): Integer; virtual; abstract;

    property ErrMsg: String read GetErrMsg;
    property Handle: HWnd read FHandle;
    property NeedTimer: Boolean read FNeedTimer;
    property NumChannels: Integer read FNumChannels write SetNumChannels;
    property SampleRate: Integer read FSampleRate write SetSampleRate;

    // ******************* NEW ****************
    property OnDataAvail: TDataAvailEvent read FOnDataAvail write FOnDataAvail;
    // ****************************************
  end;

var
  DataCollector: TDataCollector = nil;


implementation

constructor TDataCollector.Create(AHandle: HWnd);
begin
  FHandle := AHandle;
  FNumChannels := 2;
end;

function TDataCollector.GetErrMsg: String;
begin
  Result := FErrMsg;
  FErrMsg := '';
end;

function TDataCollector.GetSampleRate: Integer;
begin
  Result := FSampleRate;
end;

function TDataCollector.HasError: Boolean;
begin
  Result := FErrMsg <> '';
end;

procedure TDataCollector.SetError(const AMsg: String);
begin
  FErrMsg := AMsg;
end;

procedure TDataCollector.SetNumChannels(AValue: Integer);
begin
  FNumChannels := AValue;
end;

procedure TDataCollector.SetSampleRate(AValue: Integer);
begin
  FSampleRate := AValue;
end;

end.

