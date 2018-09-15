program ImageViewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain, Ufullscreen, UAbout,
  LCLTranslator, lazutf8;//, windows, SysUtils, StrUtils;//,Translations, LResources;//;

{$R *.res}

procedure TranslateLCL;
var
  Lang,FallbackLang: String;
begin
  // TODO: in windows, you have to call GetUserDefaultUILanguage() API to get UI languages.
  // http://sygh.hatenadiary.jp/entry/2014/05/24/181319
  {
  GetSystemDefaultLangID()
  GetUserDefaultLangID()
  GetSystemDefaultUILanguage()
  GetUserDefaultUILanguage() // We need to call this. But GetLanguageIDs calls GetUserDefaultLCID.
  GetSystemDefaultLCID()
  GetUserDefaultLCID()
  }

  Lang:='';
  FallbackLang:='';
  LazGetLanguageIDs(Lang,FallbackLang);

  //LazGetLanguageIDs(Lang,FallbackLang);
  //OutputDebugString(PChar(TrimRight( 'Lang is '+ Lang + ', FallbackLang is '+ FallbackLang )));
  //Debug Output: 'Lang is jp_JP FallbackLang is jp'
  // What??? It supporsed to be "ja_JP"!!

  //Lang := FallbackLang+'_'+Country;

  //LazGetShortLanguageID(Lang);
  //OutputDebugString(PChar(TrimRight( 'Lang is '+ Lang )));
  //Debug Output: 'Lang is jp'
  // What? not ja?

  if (Lang = 'en') or (Lang = 'en_US') or (Lang = 'us')
  or (FallbackLang = 'us') or (FallbackLang = 'US') or (FallbackLang = 'en') then
    Lang := 'en';

  if (Lang = 'ja') or (Lang = 'ja_jp') or (Lang = 'jp_JP') or (Lang = 'jp')
  or (FallbackLang = 'jp') or (FallbackLang = 'JP') or (FallbackLang = 'ja') then
    Lang := 'ja_JP';

  SetDefaultLang(Lang,'',false);

end;


begin
  TranslateLCL;
  Application.Scaled:=True;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

