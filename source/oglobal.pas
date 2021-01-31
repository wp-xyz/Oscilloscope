unit oGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TChannelIndex = (ciLeft, ciRight);

  TPlayRecordMode = (prmNone, prmPlay, prmRecord);

  TInputMode = (imRecorder, imPlayer, imWaveGenerator);

  TWaveForm = (wfSine, wfTri, wfRect);

  TChannelData = packed array[TChannelIndex] of SmallInt;
  TChannelDataArray = array of TChannelData;
  TWaveDataArray = TChannelDataArray;
  TFFTDataArray = TChannelDataArray;

  TSensitivityParams = record
    ChannelMax: Array[TChannelIndex] of Double;
    ChannelOn: Array[TChannelIndex] of Boolean;
    ChannelsLinked: Boolean;
  end;

const
  WAVE_MAX = 32767;
  WAVE_MIN = -32768;

  NUM_SAMPLES: array[0..6] of Integer = (256, 512, 1024, 2048, 4096, 8192, 16384);
  SAMPLE_RATES: array[0..3] of Integer = (44100, 48000, 96000, 192000);
  FREQUENCIES: array[0..3] of Integer = (100, 1000, 10000, 100000);

  MS_PER_DIV: array[0..8] of Double = (0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0);
  SENSITIVITIES: array[0..6] of Double = (1.0, 2.0, 5.0, 10.0, 20.0, 50.0, 100.0);

var
  SensitivityParams: TSensitivityParams = (
    ChannelMax: (100.0, 100.0);
    ChannelOn: (true, true);
    ChannelsLinked: true
  );

  NoDamageWarning_Mic: Boolean = false;
  NoDamageWarning_Out: Boolean = false;
  ShowLinesAndSymbols: Boolean = false;
  LogFrequencyAxis: Boolean = true;
  SpectrumDB: Boolean = false;


implementation

end.

