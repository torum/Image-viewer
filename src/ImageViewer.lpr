program ImageViewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain, Ufullscreen, UAbout,
  LCLTranslator, lazutf8, Translations{$ifdef windows}, Windows{$endif};

{$R *.res}

var
  ShowHelp:boolean;

procedure TranslateLCL;
var
  Lang,FallbackLang: String;
  LangID: TLanguageID;
begin
  // On windows, you have to call GetUserDefaultUILanguage() API to get UI languages.
  // But GetLanguageIDs calls GetUserDefaultLCID...

  Lang:='en';
  FallbackLang:='';
  //LazGetLanguageIDs(Lang,FallbackLang); // This is now deprecated.
  LangID := GetLanguageID;
  Lang := LangID.LanguageID;
  FallbackLang:=LangID.LanguageCode;
  // OutputDebugString(PChar(LangID.CountryCode)); // JP
  // OutputDebugString(PChar(LangID.LanguageCode)); // ja
  // OutputDebugString(PChar(LangID.LanguageID)); // ja_JP

  if (Lang = 'en') or (Lang = 'en_US') or (Lang = 'us')
  or (FallbackLang = 'us') or (FallbackLang = 'US') or (FallbackLang = 'en') then
    Lang := 'en';

  if (Lang = 'ja') or (Lang = 'ja_jp') or (Lang = 'jp_JP') or (Lang = 'jp')
  or (FallbackLang = 'jp') or (FallbackLang = 'JP') or (FallbackLang = 'ja') then
    Lang := 'ja';//'ja_JP';

  if (Lang = 'ru') or (Lang = 'ru_RU') or (Lang = 'RU')
  or (FallbackLang = 'ru') or (FallbackLang = 'RU') then
    Lang := 'ru';//'ru_RU';

  SetDefaultLang(Lang,'','',false);

end;

procedure CheckAppParam;
begin
  if (ParamCount > 0) then
  begin
    if ((ParamStr(1) = '-h') or (ParamStr(1) = '--help') or (ParamStr(1) = '/?')) then
    begin
       ShowHelp := true;
    end;
  end;
end;


begin
  CheckAppParam;

  if (ShowHelp) then
  begin
      // This creates a NEW console window to show help(-h option).
      // In order to write to original cosole, it needs to be compiled as a console application.
      // That means we need to use ($APPTYPE CONSOLE) or -WG compile/linking option.
      // However, if you do that, you get a console window WITH a normal form window ...
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
      WriteLn('  --inFrame, -s (on/off)    Start inFrame mode at startup (default off)'); 
      WriteLn('  --slideshowAutoStart, -a (on/off)    Start/force slideshow for fullscreen and inFrame mode at startup (default behavior: if single file is selected = off, if folder is selected = on)');
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
      WriteLn('');
      WriteLn('(ctrl+c to exit)');

      Application.Initialize;
      Application.ShowMainForm := False;
      Application.Run;

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

