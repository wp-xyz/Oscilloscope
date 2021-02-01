unit oBASSDataCollector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType,
  bass,
  oGlobal, oDataCollector;

type

  { TBassDataCollector }

  TBassDataCollector = class(TDataCollector)
  private
    FDevice: LongInt;
    FChannel: DWord;

  protected
    function GetErrorMsg: String;

  public
    constructor Create(AHandle: HWnd); override;
    destructor Destroy; override;

    function Open(ASampleRate, ANumChannels: Integer): boolean; override;
    procedure Close; override;

    function StartPlayback(const AFilename: String): Boolean; override; overload;
    function StartPlayback(AMemory: Pointer; ALength: DWord;
      ALoop: Boolean): Boolean; override; overload;
    function StartRecording(ASampleRate: Integer): Boolean; override;
    procedure Stop; override;
    function Running: Boolean; override;

    procedure Pause; override;
    procedure Resume; override;
    function Paused: Boolean; override;

    function GetInputType(AIndex: Integer): String; override;
    procedure GetRecordingDeviceList(AList: TStrings; out AIndex: Integer); override;
    function GetRecordingInputOn(AIndex: Integer): Boolean; override;
    function GetRecordingSensitivity(AIndex: Integer): Single; override;
    procedure SetRecordingInputActive(AIndex: Integer; Activate: Boolean;
      ASensitivity: Single = -1); override;

    function GetPlayedBytes: Int64; override;
    function GetRunningTime: Double; override;

    function GetFFTData(ABufPtr: Pointer; ANumPoints, ANumChannels: Integer): Integer; override;
    function GetWaveData(ABufPtr: Pointer; ABufSize: Integer): Integer; override;
  end;


implementation

uses
  LConvEncoding;

const
  DEFAULT_DEVICE = -1;

constructor TBassDataCollector.Create(AHandle: HWnd);
begin
  inherited;
end;

destructor TBassDataCollector.Destroy;
begin
  inherited;
end;

function TBassDataCollector.GetErrorMsg: String;
var
  err: Integer;
begin
  err := BASS_ErrorGetCode;
  case err of
    BASS_ERROR_INIT:
      Result := 'BASS_Init has not been successfully called.';
    {
    BASS_ERROR_NOTAVAIL:
      Result := 'Only decoding channels are allowed when using the "no sound" device.';
      }
    BASS_ERROR_DEVICE:
      Result := 'Illegal device number.';
    BASS_ERROR_ILLPARAM:
      Result := 'The length must be specified when streaming from memory.';
    BASS_ERROR_FILEOPEN:
      Result := 'The file could not be opened.';
    BASS_ERROR_FILEFORM:
      Result := 'The file''s format is not recognised/supported.';
    BASS_ERROR_CODEC:
      Result := 'The file uses a codec that is not available/supported. '+
        'This can apply to WAV and AIFF files, and also MP3 files when '+
        'using the "MP3-free" BASS version. ';
    BASS_ERROR_FORMAT:
      Result := 'The sample format is not supported by the device/drivers. ';
    BASS_ERROR_SPEAKER:
      Result := 'The specified SPEAKER flags are invalid. The device/drivers ' +
        'do not support them, they are attempting to assign a stereo stream '+
        'to a mono speaker or 3D functionality is enabled. ';
    BASS_ERROR_MEM:
      Result := 'There is insufficient memory.';
    BASS_ERROR_NO3D:
      Result := 'Could not initialize 3D support.';
    BASS_ERROR_BUSY:
      Result := 'The device is busy. An existing recording may need to be stopped before starting another one.';
    BASS_ERROR_NOTAVAIL:
      Result := 'The recording device is not available. Another application may '+
        'already be recording with it, or it could be a half-duplex device '+
        'that is currently being used for playback.';
    BASS_ERROR_UNKNOWN:
      Result := 'Unknown problem.';
    else
      Result := 'Error ' + IntToStr(err);
  end;
end;

procedure TBassDataCollector.Close;
begin
  BASS_RecordFree;
  BASS_Free;
end;

function TBassDataCollector.GetFFTData(ABufPtr: Pointer;
  ANumPoints, ANumChannels: Integer): Integer;
var
  flags: DWord;
begin
  case ANumPoints of
     256: flags := BASS_DATA_FFT256;
     512: flags := BASS_DATA_FFT512;
    1024: flags := BASS_DATA_FFT1024;
    2048: flags := BASS_DATA_FFT2048;
    4096: flags := BASS_DATA_FFT4096;
    8192: flags := BASS_DATA_FFT8192;
   16384: flags := BASS_DATA_FFT16384;
   else   raise Exception.Create('Only predefined number of data points allowed for FFT.');
  end;
//  flags := flags or BASS_DATA_FFT_REMOVEDC;
  if ANumChannels > 1 then
    flags := flags or BASS_DATA_FFT_INDIVIDUAL;

//  flags := flags or BASS_DATA_FFT_COMPLEX;

  Result := BASS_ChannelGetData(FChannel, ABufPtr, flags);
end;

{ Returns the position in the input stream in bytes. }
function TBassDataCollector.GetPlayedBytes: Int64;
begin
  Result := BASS_ChannelGetPosition(FChannel, BASS_POS_BYTE);
end;

{ AList is filles with names of available recording devices.
  AIndex is the index of the device which is currently on. }
procedure TBassDataCollector.GetRecordingDeviceList(AList: TStrings;
  out AIndex: Integer);
var
  i: Integer;
  pName: PChar;
  level: Single;
  flags: DWord;
  enc: String;
  s: String;
begin
  AIndex := -1;
  AList.Clear;
  i := 0;
  pName := BASS_RecordGetInputName(i);
  while pName <> nil do
  begin
    s := StrPas(pName);
    enc := GuessEncoding(s);
    AList.Add(ConvertEncoding(s, enc, EncodingUTF8));
//    AList.Add(AnsiToUtf8(StrPas(pName)));
    // is this one currently "on"?
    flags := BASS_RecordGetInput(i, level);
    if BASS_RecordGetInput(i, level) and BASS_INPUT_OFF = 0 then
      AIndex := i;
    inc(i);
    pName := BASS_RecordGetInputName(i);
	end;
end;

{ Returns true of the recording input with the specified index is on. }
function TBassDataCollector.GetRecordingInputOn(AIndex: Integer): Boolean;
var
  vol: Single = 0.0;
begin
  Result := (BASS_RecordGetInput(AIndex, vol) and BASS_INPUT_ON <> 0);
end;

function TBassDataCollector.GetInputType(AIndex: Integer): String;
var
  flags: DWord;
  vol: Single = 0.0;
begin
  flags := BASS_RecordGetInput(AIndex, vol);
  case (flags and BASS_INPUT_TYPE_MASK) of
    BASS_INPUT_TYPE_DIGITAL : Result := 'digital';
    BASS_INPUT_TYPE_LINE    : Result := 'line-in';
    BASS_INPUT_TYPE_MIC     : Result := 'microphone';
    BASS_INPUT_TYPE_SYNTH   : Result := 'midi synth';
    BASS_INPUT_TYPE_CD      : Result := 'analog cd';
    BASS_INPUT_TYPE_PHONE   : Result := 'telephone';
    BASS_INPUT_TYPE_SPEAKER : Result := 'pc speaker';
    BASS_INPUT_TYPE_WAVE    : Result := 'wave/pcm';
    BASS_INPUT_TYPE_AUX     : Result := 'aux';
    BASS_INPUT_TYPE_ANALOG  : Result := 'analog';
  else
    Result := 'undefined';
  end;
end;

function TBassDataCollector.GetRecordingSensitivity(AIndex: Integer): Single;
begin
  BASS_RecordGetInput(AIndex, Result);
end;

{ Returns the position in the audio stream, or time since START has been pressed.
  It is expected to be in seconds. }
function TBassDataCollector.GetRunningTime: Double;
var
  p: QWord;
begin
  p := BASS_ChannelGetPosition(FChannel, BASS_POS_BYTE);
  Result := BASS_ChannelBytes2Seconds(FChannel, p);
end;

{ Returns the input data and stores them in an array of int16 values.
  The array is preallocated by the calling routine.
  ABufPtr points to the first value of the array, and ABufsize is the number
  of bytes allocated for the array.
  When there are two channels the values are alternating. }
function TBassDataCollector.GetWaveData(ABufPtr: Pointer; ABufSize: Integer): Integer;
begin
  Result :=  BASS_ChannelGetData(FChannel, ABufPtr, ABufSize);
end;

function TBassDataCollector.Open(ASampleRate, ANumChannels: Integer): Boolean;
var
  flags: DWord;
  err: String;
begin
  Result := false;

  // check whether the correct BASS.DLL was loaded
  if (HiWord(BASS_GetVersion) <> BASSVERSION) then
  begin
    SetError('An incorrect version of BASS.DLL was loaded.');
    exit;
  end;

  FDevice := DEFAULT_DEVICE;

  flags := 0;
  if (not BASS_RecordInit(FDevice)) or
     (not BASS_Init(FDevice, ASampleRate, flags, {$IFNDEF MSWINDOWS}@{$ENDIF}Handle, nil)) then
  begin
    err := GetErrorMsg;
    BASS_RecordFree;
    BASS_Free;
    SetError(Format('Failure to initialize sound device (Error "%s")'+LineEnding+
      'Did you connect a microphone?', [err]));
    Exit;
  end;

  FSampleRate := ASampleRate;
  FNumChannels := ANumChannels;
  Result := true;
end;

procedure TBassDataCollector.Pause;
begin
  BASS_ChannelPause(FChannel);
end;

function TBassDataCollector.Paused: Boolean;
begin
  Result := (BASS_ChannelIsActive(FChannel) = BASS_ACTIVE_PAUSED);
end;

procedure TBassDataCollector.Resume;
begin
  BASS_ChannelPlay(FChannel, false);    // false = play from current position
end;

function TBassDataCollector.Running: Boolean;
begin
  Result := (BASS_ChannelIsActive(FChannel) = BASS_ACTIVE_PLAYING);
end;

{ Activates/deactivates the input with the given index and sets its
  sensitivity (volumne). This is a value between 0 (silent) and 1 (max).
  The default level (-1, or any other negative number) is supposed to
  leaves the volume unchanged. }
procedure TBassDataCollector.SetRecordingInputActive(AIndex: Integer;
  Activate: Boolean; ASensitivity: Single = -1);
begin
  if Activate then
    BASS_RecordSetInput(AIndex, BASS_INPUT_ON, ASensitivity)
  else
    BASS_RecordSetInput(AIndex, BASS_INPUT_OFF, ASensitivity);
end;

function RecordingCallback(Handle: HRecord; Buffer: Pointer; BufLength: DWord;
  UserPtr: Pointer): Boolean; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  // Allow recording to continue
  Result := True;
  // Data will be pulled by timer
end;

function TBassDataCollector.StartPlayback(const AFilename: String): Boolean;
const
  FROM_START = 0;
  ENTIRE_LENGTH = 0;
var
  flags: DWord;
  info: BASS_CHANNELINFO;
begin
  Result := false;
  flags := 0;
 {$IFDEF UNICODE}
  flags := flags or BASS_UNICODE;
 {$ENDIF}
  FChannel := BASS_StreamCreateFile(
    false,             // don't play from memory
    PChar(Utf8ToAnsi(AFileName)),
    FROM_START,        // play file from start
    ENTIRE_LENGTH,     // ... to end
    flags
  );
  if (FChannel = 0) then
  begin
    SetError('Could not start playback: ' + GetErrorMsg);
    exit;
  end;
  BASS_ChannelGetInfo(FChannel, info);
  FSampleRate := info.Freq;
  BASS_ChannelPlay(FChannel, true);  // true = begin from start
  Result := true;
end;

function TBassDataCollector.StartPlayback(AMemory: Pointer;
  ALength: DWord; ALoop: Boolean): Boolean;
const
  FROM_START = 0;
var
  flags: DWord;
  msg: String;
  info: BASS_CHANNELINFO;
begin
  Result := false;
  flags := 0;
 {$IFDEF UNICODE}
  flags := flags or BASS_UNICODE;
 {$ENDIF}
  if ALoop then
    flags := flags or BASS_SAMPLE_LOOP;
  FChannel := BASS_StreamCreateFile(
    true,              // play from memory
    AMemory,           // Pointer to memory location
    FROM_START,        // play file from start
    ALength,           // ... to end
    flags
  );
  if (FChannel = 0) then
  begin
    SetError('Could not start playback: ' + GetErrorMsg);
    exit;
  end;
  BASS_ChannelGetInfo(FChannel, info);
  FSampleRate := info.Freq;
  BASS_ChannelPlay(FChannel, true);  // true = begin from start
  Result := true;
end;

function TBassDataCollector.StartRecording(ASampleRate: Integer): Boolean;
var
  msg: String;
begin
  Result := False;

  FSampleRate := ASampleRate;
  FChannel := BASS_RecordStart(FSampleRate, FNumChannels, 0, RecordProc(@RecordingCallback), nil);
  if FChannel = 0 then
  begin
    SetError('Could not start recording: ' + GetErrorMsg);
    Exit;
  end;
  Result := True;
end;

procedure TBassDataCollector.Stop;
begin
  if BASS_ChannelIsActive(FChannel) = BASS_ACTIVE_STOPPED then
    exit;
  BASS_ChannelStop(FChannel);
end;

end.

