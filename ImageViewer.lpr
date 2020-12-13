program ImageViewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain, Ufullscreen, UAbout,
  LCLTranslator, lazutf8{$ifdef windows}, Windows{$endif};

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

  if (Lang = 'ru') or (Lang = 'ru_RU') or (Lang = 'RU')
  or (FallbackLang = 'ru') or (FallbackLang = 'RU') then
    Lang := 'ru_RU';

  SetDefaultLang(Lang,'',false);

end;


begin

  if (Application.ParamCount > 0) then
  begin
    if ((Application.Params[0] = '-h') or (Application.Params[0] = '--help') or (Application.Params[0] = '/?')) then
    begin

      // This creates a NEW console window.
      // In order to write to original cosole is to compile as a console application
      // using ($APPTYPE CONSOLE) or -WG compile/linking option.
      // However, if you do, you get a console window with a normal form window when execute...
      {$ifdef windows}
      AllocConsole;      // in Windows unit
      {$endif}
      IsConsole := True; // in System unit
      SysInitStdIO;      // in System unit

      WriteLn('ImageViewer - a simple image viewer.');
      WriteLn('');
      WriteLn('Usage:');
      WriteLn('ImageViewer [OPTION]=[VALUE] or ImageViewer [OPTION] [VALUE]');
      WriteLn('');
      WriteLn('Options and Values:');
      WriteLn('  --fullscreen, -f (on/off)    Start fullscreen at startup (default off)');
      WriteLn('  --interval, -i (#)    Slideshow interval in seconds (default 4 seconds)');
      WriteLn('  --random, -r (on/off)    Slideshow random (default on)');
      WriteLn('  --repeat, -e (on/off)    Slideshow repeat (default on)');
      WriteLn('  --effect, -t (on/off)    Slideshow transitional effect (default on)');
      WriteLn('  --stretchIn, -n (on/off)    Picture stretch In -fit to window/screen when the size is bigger. (default on)');
      WriteLn('  --stretchOut, -o (on/off)    Picture stretch Out -fit to window/screen when the size is smaller. (default off)');
      WriteLn('  --includSubFolders, -u (on/off)    Loads pictures in the sub folders as well when manually select a picture (default on)');
      WriteLn('  --moniter, -m (#)    Specify a moniter to show fullscreen slideshow (0 is the default main moniter');
      WriteLn('  --stayOnTop, -y (on/off)    Specify if window should stay on top (default off)');
      WriteLn('  --help, -h    Shows this.');
      WriteLn('');
      WriteLn('Useage example:');
      WriteLn('  $ ImageViewer -f on -i 2 -o on -e off C:\Users\<USER>\Pictures\Wallpapers\');

      Application.Initialize;
      Application.ShowMainForm := False;
      Application.Run;

    end;

  end else
  begin
    TranslateLCL;
    Application.Scaled:=True;
    RequireDerivedFormResource:=True;
    Application.Initialize;
    Application.ShowMainForm := False;
    Application.CreateForm(TfrmMain, frmMain);
    Application.Run;
  end;

end.

