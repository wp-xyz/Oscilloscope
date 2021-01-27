program Oscilloscope;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uecontrols, tachartlazaruspkg, oMain, oBaseFrame, oGlobal,
  oDataCollector, oBASSDataCollector, oOscilloscopeFrame, oSpectrumFrame,
  oUtils, oWav
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

