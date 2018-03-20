program ImageViewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain, Ufullscreen, UAbout;
  //LCLTranslator, Translations, lazutf8, SysUtils, StrUtils, LResources;

{$R *.res}


{
procedure TranslateLCL;
var
  PODirectory, Filename,Lang, FallbackLang: String;
begin
  PODirectory:=ExtractFilePath(ParamStr(0))+'languages';
  PODirectory:=IncludeTrailingPathDelimiter(PODirectory);
  Filename := ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');
  Lang:='en';
  FallbackLang:='en';
  //LazGetLanguageIDs(Lang,FallbackLang); // in unit lazutf8
  LazGetShortLanguageID(Lang);
  if Lang = '' then exit;
  // http://wiki.freepascal.org/Language_Codes
  // bug?
  if Lang = 'jp' then
    Lang := 'ja';

  if FileExists(PODirectory+'LCLStrConsts'+'.'+Lang+'.po') then
  begin
    Translations.TranslateUnitResourceStrings('LCLStrConsts',
                        PODirectory+'lclstrconsts.%s.po',Lang,FallbackLang);
  end;

  if FileExists(PODirectory+Filename+'.'+Lang+'.po') then
  begin
    Translations.TranslateResourceStrings(
                        PODirectory+Filename+'.%s.po',Lang,FallbackLang);
  end;


end;
}

begin
  //TranslateLCL;
  Application.Scaled:=True;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

