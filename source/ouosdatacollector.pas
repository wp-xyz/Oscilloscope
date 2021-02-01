unit ouosDataCollector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType,
  uos_flat,
  oDataCollector;

type

  { TuosDataCollector }

  TuosDataCollector = class(TDataCollector)
  private
    FDevice: longint;
    FChannel: DWord;

  protected
    function GetErrorMsg: string;

    // ********* NEW ********
    procedure DataAvailProc; override;  // this must be used as argument in uos_LoopProcIn
    // **********************

  public
    constructor Create(AHandle: HWnd); override;
    destructor Destroy; override;

    function Open(ASampleRate, ANumChannels: integer): Boolean; override;
    procedure Close; override;

    function StartPlayback(const AFilename: string): Boolean; override; overload;
    function StartPlayback(AMemory: Pointer; ALength: DWord; ALoop: Boolean): Boolean; override; overload;
    function StartRecording(ASampleRate: integer): Boolean; override;
    procedure Stop; override;
    function Running: Boolean; override;

    procedure Pause; override;
    procedure Resume; override;
    function Paused: Boolean; override;

    function GetInputType(AIndex: integer): string; override;
    procedure GetRecordingDeviceList(AList: TStrings; out AIndex: integer); override;
    function GetRecordingInputOn(AIndex: integer): Boolean; override;
    function GetRecordingSensitivity(AIndex: integer): single; override;
    procedure SetRecordingInputActive(AIndex: integer; Activate: Boolean; ASensitivity: single = -1); override;

    function GetPlayedBytes: int64; override;
    function GetRunningTime: double; override;

    function GetFFTData(ABufPtr: Pointer; ANumPoints, ANumChannels: integer): integer; override;
    function GetWaveData(ABufPtr: Pointer; ABufSize: integer): integer; override;
  end;

var
  BData: array of single;

implementation

uses
  LConvEncoding, LazFileUtils, Forms,
  omain;

const
  DEFAULT_DEVICE = -1;

var
  erruos: integer = 0;

constructor TuosDataCollector.Create(AHandle: HWnd);
begin
  inherited;
end;

destructor TuosDataCollector.Destroy;
begin
  inherited;
end;

function TuosDataCollector.GetErrorMsg: string;
begin
  case erruos of
    1: Result := 'Some uos libraries are not loaded';
    2: Result := 'Player failed to create';
    3: Result := 'Output device not created';
  end;
end;

procedure TuosDataCollector.Close;
begin
  uos_free;
end;

//***********NEW******************************************
procedure TuosDataCollector.DataAvailProc;
var
  P: Pointer;
  N: Integer;
begin
  if Assigned(OnDataAvail) then
  begin
    //  P := @ (first byte in uosbuffer)
    //  N := ( number of bytes in uosbuffer);
    OnDataAvail(P, N);
  end;
end;
// *********************************************************

function TuosDataCollector.GetFFTData(ABufPtr: Pointer; ANumPoints, ANumChannels: integer): integer;
var
  i, x: integer;
  bufferwav: TDArFloat;
begin
  bufferwav := uos_InputGetBuffer(FChannel, 0);

  setlength(BData, ANumPoints);

  x := 0;
  for i := 0 to length(BData) - 1 do
  begin
    BData[i] := bufferwav[x] ;
    inc(x, FnumChannels);
   end; 

  Result := 0;
end;

function TuosDataCollector.GetPlayedBytes: int64;
begin
  Result := uos_InputPosition(0, 0);
end;

{ AList is filles with names of available recording devices.
  AIndex is the index of the device which is currently on. }
procedure TuosDataCollector.GetRecordingDeviceList(AList: TStrings; out AIndex: integer);
begin
//  
end;

{ Returns true of the recording input with the specified index is on. }
function TuosDataCollector.GetRecordingInputOn(AIndex: integer): Boolean;
begin
Result := False;
end;

function TuosDataCollector.GetInputType(AIndex: integer): string;
begin
result := '';
end;

function TuosDataCollector.GetRecordingSensitivity(AIndex: integer): single;
begin
Result := 0.0;
end;

function TuosDataCollector.GetRunningTime: double;
begin
  Result := uos_InputPositionSeconds(FChannel, 0);
  //Result := 10.0;
end;

// ABufPtr points to beginning of pre-allocated buffer. ABufSize is the size
// of the allocated buffer in bytes.
// TOscilloscopeFrame assumes that the buffer is an array of SmallInt values
function TuosDataCollector.GetWaveData(ABufPtr: Pointer; ABufSize: integer): integer;
var
  i, nBuf: integer;
  bufferwav: TDArFloat;
  EndPtr: Pointer;
  intValue: Integer;
begin
  Result := -1;

  bufferwav := uos_InputGetBuffer(FChannel, 0);  // wp: seems to be correct length, but how can we be sure that this returns the amount needed for the buffer?
  nBuf := Length(bufferwav);

  EndPtr := ABufPtr + ABufSize;
  i := 0;
  while (ABufPtr < EndPtr) and (i < nBuf) do
  begin
    intValue := round(bufferwav[i] * 32768);
    if intValue > 32767 then
      intValue := 32767
    else if intValue < -32768 then
      intValue := -32768;
    PSmallInt(ABufPtr)^ := SmallInt(intValue);
    inc(ABufPtr, 2);
    inc(i);
  end;
  Result := i * 2;
end;

function TuosDataCollector.Open(ASampleRate, ANumChannels: integer): Boolean;
var
  binDir, pafn, sffn, mpfn, ordir, opath: string;
begin
  Result := False;

  binDir := AppendPathDelim(Application.Location);
 {$IFNDEF Windows}
  //  ordir := './3rdParty/uos/';
   ordir := './';
 {$ENDIF}

 {$IFDEF Windows}
  {$if defined(cpu64)}
  pafn := binDir + 'LibPortaudio-64.dll';
  sffn := binDir + 'LibSndFile-64.dll';
  mpfn := binDir + 'LibMpg123-64.dll';
  {$else}
  pafn := binDir + 'LibPortaudio-32.dll';
  sffn := binDir + 'LibSndFile-32.dll';
  mpfn := binDir + 'LibMpg123-32.dll';
  {$endif}
 {$ENDIF}

  {$IFDEF Darwin}
   {$IFDEF CPU32}
    opath := ordir;
    opath := copy(opath, 1, Pos('/uos', opath) - 1);
    pafn := opath + '/lib/Mac/32bit/LibPortaudio-32.dylib';
    sffn := opath + '/lib/Mac/32bit/LibSndFile-32.dylib';
    mpfn := opath + '/lib/Mac/32bit/LibMpg123-32.dylib';
    {$ENDIF}
    {$IFDEF CPU64}
    opath := ordir;
    opath := copy(opath, 1, Pos('/uos', opath) - 1);
    pafn := opath + '/lib/Mac/64bit/LibPortaudio-64.dylib';
    sffn := opath + '/lib/Mac/64bit/LibSndFile-64.dylib';
    {$ENDIF}
   {$ENDIF}

 {$if defined(CPUAMD64) and defined(linux) }
  pafn := binDir + 'LibPortaudio-64.so';
  sffn := binDir + 'LibSndFile-64.so';
  mpfn := binDir + 'LibMpg123-64.so';
 {$ENDIF}

 {$if defined(cpu32) and defined(linux) and not defined(cpuarm)}
  pafn := binDir + 'LibPortaudio-32.so';
  sffn := binDir + 'LibSndFile-32.so';
  mpfn := binDir + 'LibMpg123-32.so';
 {$ENDIF}

 {$if defined(linux) and defined(cpuarm)}
  pafn := ordir + 'lib/Linux/arm_raspberrypi/libportaudio-arm.so';
  sffn := ordir + 'lib/Linux/arm_raspberrypi/libsndfile-arm.so';
  mpfn := ordir + 'lib/Linux/arm_raspberrypi/libmpg123-arm.so';
 {$ENDIF}

   {$if defined(linux) and defined(cpuaarch64)}
   pafn := ordir + 'lib/Linux/aarch64_raspberrypi/libportaudio_aarch64.so';
   sffn := ordir + 'lib/Linux/aarch64_raspberrypi/libsndfile_aarch64.so';
   mpfn := ordir + 'lib/Linux/aarch64_raspberrypi/libmpg123_aarch64.so';
   {$ENDIF}

  {$IFDEF freebsd}
    {$if defined(cpu64)}
    pafn := ordir + 'lib/FreeBSD/64bit/libportaudio-64.so';
    sffn := ordir + 'lib/FreeBSD/64bit/libsndfile-64.so';
    mpfn := ordir + 'lib/FreeBSD/64bit/libmpg123-64.so';
    {$else}
    pafn := ordir + 'lib/FreeBSD/32bit/libportaudio-32.so';
    sffn := ordir + 'lib/FreeBSD/32bit/libsndfile-32.so';
    mpfn := ordir + 'lib/FreeBSD/32bit/libmpg123-32.so';
   {$endif}
  {$ENDIF}
  
  if uos_LoadLib(PChar(pafn), PChar(sffn), PChar(mpfn), nil, nil, nil) <> -1 then
  begin
    FSampleRate  := ASampleRate;
    FNumChannels := ANumChannels;
    Result       := True;
    erruos       := 0;
  end
  else
    erruos       := 1;
end;

procedure TuosDataCollector.Pause;
begin
  uos_pause(FChannel);
end;

function TuosDataCollector.Paused: Boolean;
begin
  Result   := False;
  if uos_GetStatus(FChannel) = 2 then
    Result := True;
end;

procedure TuosDataCollector.Resume;
begin
  uos_replay(FChannel);
end;

function TuosDataCollector.Running: Boolean;
begin
  Result   := False;
  if uos_GetStatus(FChannel) = 1 then
    Result := True;
end;

procedure TuosDataCollector.SetRecordingInputActive(AIndex: integer; Activate: Boolean; ASensitivity: single = -1);
begin

end;

function RecordingCallback(Handle: Pointer; Buffer: Pointer; BufLength: DWord; UserPtr: Pointer): Boolean; {$IFDEF MSWINDOWS} stdcall{$ELSE}cdecl{$ENDIF};
begin
   Result := True;
 end;

function TuosDataCollector.StartPlayback(const AFilename: string): Boolean;
begin
  Result   := False;
  FChannel := 0;
  if uos_CreatePlayer(FChannel) then
    if uos_AddIntoDevOut(FChannel, -1, -1, FSampleRate, FNumChannels, 0, 2048 * FNumChannels, -1) <> -1 then
      if uos_AddFromFile(FChannel, PChar(AFileName), -1, 0, 2048 * FNumChannels) > -1 then
      begin
       uos_InputSetPositionEnable(FChannel, 0, 1);
       Result := True;
        uos_play(FChannel);
      end;
end;

function TuosDataCollector.StartPlayback(AMemory: Pointer; ALength: DWord; ALoop: Boolean): Boolean;
var
  wavetype: shortint;
begin
  Result     := False;
  if mainform.BtnSineWave.down then
    wavetype := 0
  else if mainform.BtnTriangWave.down then
    wavetype := 2
  else
    wavetype := 1;
  FChannel := 0;
  uos_stop(FChannel);
  if uos_CreatePlayer(FChannel) then
    if uos_AddIntoDevOut(FChannel, -1, -1,
      StrToInt(mainform.CbGeneratorSampleRate.Caption), 1, 0, 1024, -1) <> -1 then
      if uos_AddFromSynth(FChannel, 1, wavetype, wavetype,
        (mainform.EdFrequency.Value * 1000),
        (mainform.EdFrequency.Value * 1000),
        (mainform.edvolume.Value / 100),
        (mainform.edvolume.Value / 100), 0, -1, -1, -1, 0, -1, 1024) <> -1 then
      begin
        Result := True;
        uos_play(FChannel);
      end;
end;

function TuosDataCollector.StartRecording(ASampleRate: integer): Boolean;
var
  msg: string;
begin
  Result   := False;
  FChannel := 0;
  FSampleRate := ASampleRate;
  if uos_CreatePlayer(FChannel) then
    if uos_AddFromDevIn(FChannel, -1, -1, ASampleRate, -1, 0, 2048, -1 )  > -1 then
      begin
        Result := True;
        uos_play(FChannel);
      end;  
end;

procedure TuosDataCollector.Stop;
begin
  uos_stop(FChannel);
end;

end.

