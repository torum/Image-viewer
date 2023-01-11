unit UMain;


// {$mode objfpc}{$H+}
// EnableBlur procedure requires delphi mode.
{$mode delphi}

{
Source:
 https://github.com/torum/Image-viewer

No extra components required.

Compiled and tested on
 Windows 11: Lazarus 2.2.2 FPC 3.2.2 x86_64-win64-win32/win64
 Windows 10: Lazarus 1.8.0 r56594 FPC 3.0.4 x86_64-win64-win32/win64
 Ubuntu 22.04.1 LTS: Lazarus 2.2.0 FPC 3.2.2 x86_64-linux-gtk2
 Ubuntu 17.10 (64bit): Lazarus 1.8.0 rc4+dfsg-1 FPC 3.0.2 x86_64-linux-gtk2
 Ubuntu 16.04 LTS (64bit): Lazarus 1.9.0 trunk, FPC 3.0.4
 macOS 10.13.3 (64bit) High Sierra: Lazarus 1.8.0 rexported FPC 3.0.4 i386-darwin-carbon
 macOS 10.11.6 (64bit) El Capitan: Lazarus 1.9.0 carbon trunk, FPC 3.0.4

Known issues and bugs:
 On Windows, inFrame "window" does not have shaddow.
 On Windows, PNG (depth 24) antialising isn't working when stretch.
  https://forum.lazarus.freepascal.org/index.php?topic=24408.0
  http://forum.lazarus.freepascal.org/index.php?topic=19542.0
 On Ubuntu, inFrame transit effect doesn't seem to be working..
 On macOS, inFrame transit effect won't work?
 On macOS El Capitan, the top bar won't hide. It's fine on High Sierra.
 On macOS, trayicon won't show up correctly. Black filled.->disabled
 On macOS, awaking from sleep >blank screen?
 Cocoa based 64bit apps for macOS may not be ready some time soon...

TODO:
 preLoading image for slideshow.
 load playlist.
 more Command line options.
 Modal dialog with options and playlist edit tab.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LclType, LclProc, LclIntf, Menus, StdCtrls, ExtDlgs,
  strutils, Types, FileCtrl, XMLConf{$ifdef windows}, windirs, Windows, DWMApi{$endif};

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Image1: TImage;
    MenuItemAbout: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemBackgroundBlack: TMenuItem;
    MenuItemBackgroundWhite: TMenuItem;
    MenuItemBackgroundColor: TMenuItem;
    MenuItemBo8: TMenuItem;
    MenuItemSysAbout: TMenuItem;
    MenuItemSysBo: TMenuItem;
    MenuItemSysQuit: TMenuItem;
    MenuItemStretchIn: TMenuItem;
    MenuItemStretchOut: TMenuItem;
    MenuItemBo7: TMenuItem;
    MenuItemNext: TMenuItem;
    MenuItemBack: TMenuItem;
    MenuItemBo6: TMenuItem;
    MenuItemStretch: TMenuItem;
    MenuItemStayOnTop: TMenuItem;
    MenuItemSlideshow: TMenuItem;
    MenuItemBo9: TMenuItem;
    MenuItemSlideshowInFrame: TMenuItem;
    MenuItemQuit: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    PaintBox1: TPaintBox;
    PopupMenuSystem: TPopupMenu;
    PopupMenuMain: TPopupMenu;
    ScrollBox1: TScrollBox;
    TimerEffectStart: TTimer;
    TrayIcon1: TTrayIcon;
    XMLConfig: TXMLConfig;
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Image1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Image1Resize(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemBackgroundBlackClick(Sender: TObject);
    procedure MenuItemBackgroundWhiteClick(Sender: TObject);
    procedure MenuItemNextClick(Sender: TObject);
    procedure MenuItemBackClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemSlideshowClick(Sender: TObject);
    procedure MenuItemSlideshowInFrameClick(Sender: TObject);
    procedure MenuItemStayOnTopClick(Sender: TObject);
    procedure MenuItemStretchInClick(Sender: TObject);
    procedure MenuItemStretchOutClick(Sender: TObject);
    procedure MenuItemSysAboutClick(Sender: TObject);
    procedure MenuItemSysQuitClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PopupMenuMainPopup(Sender: TObject);
    procedure PopupMenuSystemPopup(Sender: TObject);
    procedure ScrollBox1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure TimerEffectStartTimer(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);

  private
    // Main file list.
    FstFileList:TStringList;
    FstDirectoryList:TStringList;
    FstPlaylistList:TStringList;
    FstFileExtList:TStringList;
    FstPlaylistExtList:TStringList;
    // User Opts.
    FOptFullscreen:boolean;
    FOptTransitEffect:boolean;
    FOptFit:boolean;
    FOptExpand:boolean;
    FOptIntervalIntSeconds:integer;
    FOptMinimulFileSizeKiloByte:integer;
    FOptRepeat:boolean;
    FOptRandom:boolean;
    FoptStayOnTop:boolean;
    FoptBackgroundBlack:boolean;
    FOptFileExts:string;
    FOptPlaylistExts:string;
    FOptIncludeSubFolders:boolean;
    FOptIntMoniter:integer;
    // App status.
    FisFullScreen: boolean;
    FisStartNormal: boolean;
    FisStretch:boolean;
    FisInFrame:boolean;
    FisSlideshowAutoStart:boolean;
    FisSingleFileSelected: boolean;
    FisManualTransition:boolean;
    FCurrentMonitor:TMonitor;
    FOrigBounds: TRect;
    FOrigWndState: TWindowState;
    FiCurrentFileIndex:integer;
    FstrAppVer:string;
    FstrInitialSelectedImageFile:string;
    FstrInitialDir:string;
    FintZoomingScaleFactor:integer;
    procedure ShowFullScreen(blnOn: boolean);
    procedure SetFullScreen_Universal(blnOn: boolean);
    procedure SetFullScreen_Win32(blnOn: boolean);
    procedure SetMoniter(intMoniter:integer);
    procedure SetStayOnTop(bln:boolean);
    function GetCurrentMonitor():TMonitor;
    function GetCurrentMonitorIndex():integer;
    procedure RestoreFormState;
    procedure StoreFormState;
    procedure LoadImage;
    function DispayImage(filename:string):boolean;
    procedure DrawImage;
    procedure LoadDirectories(const Dirs: TStringList; FList: TStringList);
    procedure LoadSiblings(const FName: string; FList: TStringList);
    {$ifdef windows}
    procedure EnableBlur;
    //procedure AeroGlass;
    {$endif}
  public
    property FileList: TStringList read FstFileList;
    // User options. Read/Write.
    property OptTransitEffect: boolean read FoptTransitEffect write FoptTransitEffect;
    property OptFit: boolean read FOptFit write FOptFit;
    property OptExpand: boolean read FOptExpand write FOptExpand;
    property OptIntMoniter: integer read FOptIntMoniter write SetMoniter;
    property OptIntervalIntSeconds: integer read FOptIntervalIntSeconds write FOptIntervalIntSeconds;
    property OptRandom: boolean read FOptRandom write FOptRandom;
    property OptRepeat: boolean read FOptRepeat write FOptRepeat;
    property OptStayOnTopInframe:boolean read FoptStayOnTop write SetStayOnTop;
    // Misc flags and props.
    property OptMinimulFileSizeKiloByte: integer read FOptMinimulFileSizeKiloByte;
    property OptStretch: boolean read FisStretch;
    property CurrentMonitor:TMonitor read GetCurrentMonitor;
    property IsInFrame:boolean read FisInFrame;
    property IsStartNormal:boolean read FisStartNormal;
    property OptSlideshowAutoStart:boolean read FisSlideshowAutoStart;
    property IsSingleFileSelected: boolean read FisSingleFileSelected;
    property IsManualTransition: boolean read FisManualTransition;
    // Called from inFrame.
    procedure DoneInFrame(strCurr:string);
    // Called from fullscreen.
    procedure DoneFullscreen(strCurr:string);
    // Called from inFrame.
    procedure SetCaption(strCaption:string);
  end;

  {$ifdef windows}
  AccentPolicy = packed record
    AccentState: Integer;
    AccentFlags: Integer;
    GradientColor: Integer;
    AnimationId: Integer;
  end;

  TWinCompAttrData = packed record
    attribute: THandle;
    pData: Pointer;
    dataSize: ULONG;
  end;
  {$endif}

var
  frmMain: TfrmMain;

{$ifdef windows}
var
  SetWindowCompositionAttribute: function (Wnd: HWND; const AttrData: TWinCompAttrData): BOOL; stdcall = Nil;
{$endif}

resourcestring
  resstrStretch = 'Stretch';
  resstrStretchIn = 'In (big->screen)';
  resstrStretchOut = 'Out (small->screen)';
  resstrStayOnTop = 'Stay on top';
  resstrQuit = '&Quit';
  resstrAbout = 'About';

  resstrPlayback = '&Playback';
  resstrPlaybackNext = '&Next';
  resstrPlaybackPrevious = '&Back';
  resstrPlaybackPause = 'Pau&se';
  resstrInterval = '&Interval';
  resstrSecond = 'second';
  resstrSeconds = 'seconds';
  resstrMinute = 'minute';
  resstrMinutes = 'minutes';
  resstrRandom = 'Ran&dom';
  resstrRepeat = 'Repe&at';
  resstrEffect = '&Effect';
  resstrFilter = '&Filter';
  resstrFilterFileSize = 'File size';
  resstrMoniters = '&Moniters';
  resstrMoniter = 'Moniter';
  resstrLeaveInFrame = '&Leave InFrame';
  resstrStopInFrameSlideshow = '&Stop InFrame Slideshow';
  resstrLeaveFullscreen = '&Leave Fullscreen';
  resstrStopFullscreenSlideshow = '&Stop Fullscreen Slideshow';
  resstrClose = '&Close';

  resstrInFrameSlideshow = 'InFrame Slideshow';
  resstrFullscreenSlideshow = 'Fullscreen Slideshow';
  resstrInFrameView = '&InFrame View';
  resstrFullscreenView = '&Fullscreen View';
  resstrStartInFrameSlideshow = 'Start &InFrame Slideshow';
  resstrStartFullscreenSlideshow = 'Start &Fullscreen Slideshow';

  resstrOpenPictures = 'Open picture(s)';

  resstrBackground = 'Background Color';
  resstrBackgroundBlack = 'Black';
  resstrBackgroundWhite = 'White';

implementation

uses UFullscreen, UAbout;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.LoadDirectories(const Dirs: TStringList; FList: TStringList);
var
  i,j,f:integer;
  fileSearchMask:string;
  folderfiles:TStringlist;
begin
  if Dirs.Count > 0 then
  begin
    // Create search mask
    fileSearchMask:='';
    for i:=0 to FstFileExtList.Count-1 do
    begin
      if trim(FstFileExtList[i]) <> '' then
      begin
         fileSearchMask:= fileSearchMask+'*'+trim(FstFileExtList[i])+';';
      end;
    end;
    // Loop directories and FindAllFiles.
    for i:=0 to Dirs.Count -1 do
    begin
      try
        // Recursively search files
        folderfiles := FindAllFiles(Dirs[i], fileSearchMask, FOptIncludeSubFolders);
        for j:=0 to folderfiles.Count - 1 do
        begin
          // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
          if not (AnsiStartsStr('.',ExtractFilename(folderfiles[j]))) then
          begin
            f:= FileSize(folderfiles[j]);
            // Check file size.
            if f >= (FOptMinimulFileSizeKiloByte) then
            begin
              FList.Add(folderfiles[j]);
            end;
          end;
        end;
      finally
        folderfiles.Free;
      end;
    end;
  end;
end;

procedure TfrmMain.LoadSiblings(const FName: string; FList: TStringList);
var
  i,j,f:integer;
  fileSearchMask, fileFolder:string;
  folderfiles:TStringlist;
begin
  fileFolder:=ReplaceStr(FName,ExtractFileName(FName),'');
  //fileFolder:=ReplaceStr(FstrInitialSelectedImageFile,ExtractFileName(FstrInitialSelectedImageFile),'');

  // sets init dir for next time.
  FstrInitialDir := ExtractFilePath(FName);

  // Create search mask
  fileSearchMask:='';
  for i:=0 to FstFileExtList.Count-1 do
  begin
    if trim(FstFileExtList[i]) <> '' then
    begin
       fileSearchMask:= fileSearchMask+'*'+trim(FstFileExtList[i])+';';
    end;
  end;

  try
    // Find siblings.
    folderfiles := FindAllFiles(fileFolder, fileSearchMask, false);
    for j:=0 to folderfiles.Count - 1 do
    begin
      // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
      if not (AnsiStartsStr('.',ExtractFilename(folderfiles[j]))) then
      begin
        // Ignore first selected image.
        if (folderfiles[j] <> FName) then
        begin
          f:= FileSize(folderfiles[j]);
          // Check file size.
          if f >= (FOptMinimulFileSizeKiloByte) then
          begin
            FList.Add(folderfiles[j]);
          end;
        end else
        begin
          // remove firest selected and add to list so that file order is right.
          FList.Delete(0);
          FList.Add(folderfiles[j]);
        end;
      end;
    end;
  finally
    folderfiles.Free;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  s:string;
  i,f:integer;
  configFile:string;
begin
  FstrAppVer:='1.3.4.1';

  // Init Main form properties.
  self.Caption:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');
  self.Visible:=false;
  self.AlphaBlend:=true;
  self.AlphaBlendValue:=255;

  TimerEffectStart.Enabled:=false;

  // Init main lists.
  FstFileList:=TStringList.create;
  FstDirectoryList:=TStringList.create;
  FstPlaylistList:=TstringList.Create;

  // Set defaults for user options.
  FoptBackgroundBlack:=true;
  FOptFullscreen:=false;
  FOptTransitEffect:=true;
  FOptExpand:=false;
  FOptFit:=true;
  FOptIntMoniter:=0;
  FOptIntervalIntSeconds:=4;
  FOptRandom:=true;
  FOptRepeat:=true;
  {$ifdef windows}
  FOptFileExts:='.jpg;.jpeg;.jpe;.jfif;.png;.gif;.bmp;.ico';
  OpenPictureDialog1.Filter:='All (*.jpeg;*.jpg;*.jpe;*.jfif;*.png;*.gif;*.bmp;*.ico)|*.jpeg;*.jpg;*.jpe;*.jfif;*.png;*.gif;*.bmp;*.ico|Joint Picture Expert Group (*.jpeg;*.jpg;*.jpe;*.jfif)|*.jpeg;*.jpg;*.jpe;*.jfif|Portable Network Graphics (*.png)|*.png|Graphics Interchange Format (*.gif)|*.gif|Bitmap (*.bmp)|*.bmp|Icon (*.ico)|*.ico';
  {$else}
  FOptFileExts:='.jpg;.jpeg;.jpe;.png;.gif';
  {$endif}
  FOptPlaylistExts:='.m3u;.xspf';
  FOptIncludeSubFolders:=true;
  FoptStayOnTop:=false;
  // Be carefull when settings this. If the size is too large, the list will be empty.
  FOptMinimulFileSizeKiloByte:=1;

  // Non user editable flags.
  FisSlideshowAutoStart:=true;
  FisStretch:=false;
  FisInFrame:=false;

  // Init other objects.
  FstFileExtList := TStringList.Create;
  FstFileExtList.Delimiter:=';';
  FstFileExtList.DelimitedText:=FOptFileExts;

  FstPlaylistExtList:= TStringList.Create;
  FstPlaylistExtList.Delimiter:=';';
  FstPlaylistExtList.DelimitedText:=FOptPlaylistExts;

  if Screen.MonitorCount < 1 then
    FOptIntMoniter:=0;

  FstrInitialSelectedImageFile :='';

  FintZoomingScaleFactor := 1;

  {$ifdef windows}
  FstrInitialDir := GetWindowsSpecialDir(CSIDL_MYPICTURES);
  {$else}
  // TODO:
  FstrInitialDir := '';
  {$endif}

  // i18n
  MenuItemNext.Caption := resstrPlaybackNext;
  MenuItemBack.Caption := resstrPlaybackPrevious;
  MenuItemStretch.Caption := resstrStretch;
  MenuItemStretchIn.Caption := resstrStretchIn;
  MenuItemStretchOut.Caption := resstrStretchOut;
  MenuItemStayOnTop.Caption := resstrStayOnTop;
  MenuItemSlideshowInFrame.Caption := resstrInFrameSlideshow;
  MenuItemSlideshow.Caption := resstrFullscreenSlideshow;
  MenuItemQuit.Caption := resstrQuit;
  MenuItemSysAbout.Caption := resstrAbout;
  MenuItemAbout.Caption := resstrAbout;
  MenuItemSysQuit.Caption := resstrQuit;

  OpenPictureDialog1.Title:=resstrOpenPictures;

  MenuItemBackgroundColor.Caption:=resstrBackground;
  MenuItemBackgroundBlack.Caption:=resstrBackgroundBlack;
  MenuItemBackgroundWhite.Caption:=resstrBackgroundWhite;

  // Load settings
  configFile := ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'.config');
  if (not FileExists(ExtractFilePath(Application.ExeName)+configFile)) and ForceDirectories(GetAppConfigDir(false)) then
  begin
    //XMLConfig.FileName:=GetAppConfigFile(False); // exename.cfg
    {$ifdef windows}
    XMLConfig.FileName:=GetAppConfigDir(false)+configFile;
    {$else}
    XMLConfig.FileName:=GetAppConfigDir(false)+'.'+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'') +'.config';
    {$endif}
  end else begin
    {$ifdef windows}
    XMLConfig.FileName:=ExtractFilePath(Application.ExeName)+configFile;
    {$else}
    XMLConfig.FileName:='.'+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'') +'.config';
    {$endif}
  end;

  FOptRandom := XMLConfig.GetValue('/Opts/Random',FOptRandom);
  FOptRepeat := XMLConfig.GetValue('/Opts/Repeat',FOptRepeat);
  //FOptFullscreen := XMLConfig.GetValue('/Opts/Fullscreen',FOptFullscreen);
  FOptTransitEffect := XMLConfig.GetValue('/Opts/TransitEffect',FOptTransitEffect);
  FOptFit := XMLConfig.GetValue('/Opts/Fit',FOptFit);
  FOptExpand := XMLConfig.GetValue('/Opts/Expand',FOptExpand);
  FOptIntMoniter := XMLConfig.GetValue('/Opts/Moniter',FOptIntMoniter);
  // Don't load FOptMinimulFileSizeKiloByte...  If the size is too large, the list will be empty.
  //FOptMinimulFileSizeKiloByte := XMLConfig.GetValue('/Opts/MinimulFileSizeKiloByte',FOptMinimulFileSizeKiloByte);
  FoptStayOnTop := XMLConfig.GetValue('/Opts/StayOnTop',FoptStayOnTop);
  FoptBackgroundBlack := XMLConfig.GetValue('/Opts/BackgroundBlack',FoptBackgroundBlack);
  FOptIncludeSubFolders := XMLConfig.GetValue('/Opts/IncludeSubFolders',FOptIncludeSubFolders);
  FOptFileExts := string(XMLConfig.GetValue('/Opts/FileExts',widestring(FOptFileExts)));
  FOptPlaylistExts := string(XMLConfig.GetValue('/Opts/PlaylistExts',widestring(FOptPlaylistExts)));
  FOptIntervalIntSeconds := XMLConfig.GetValue('/Opts/IntervalSeconds',FOptIntervalIntSeconds);

  FstrInitialDir := string(XMLConfig.GetValue('/InitDir/Path',widestring(FstrInitialDir)));

  // Command line parameters
  if Application.HasOption('h', 'help') then
  begin
    // -h is a short option. The long form is the --help

    // handled at .lpr
    //frmAbout := TfrmAbout.Create(self);
    //frmAbout.Caption:=' '+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');
    //frmAbout.StaticTextAppsVer.Caption := 'Image Viewer' + ' - ' + FstrAppVer;
    //frmAbout.StaticTextWho.Caption := 'by torum';
    //frmAbout.StaticTextWebSite.Caption:='https://torum.github.io/Image-viewer/';
    //frmAbout.ShowModal;
    //Halt;
  end;
  // "-o off" or "--option=off" on/off for a boolean value.
  if Application.HasOption('i', 'interval') then
  begin
    s := trim(Application.GetOptionValue('i', 'interval'));
    try
      i := strToInt(s);
    except
      i := FOptIntervalIntSeconds;
    end;
    FOptIntervalIntSeconds := i;
  end;
  if Application.HasOption('r', 'random') then
  begin
    if (LowerCase(Application.GetOptionValue('r', 'random')) = 'on') then
    begin
      FOptRandom:=true;
    end else if (LowerCase(Application.GetOptionValue('r', 'random')) = 'off') then
    begin
      FOptRandom:=false;
    end;
  end;
  if Application.HasOption('e', 'repeat') then
  begin
    if (LowerCase(Application.GetOptionValue('e', 'repeat')) = 'on') then
    begin
      FOptRepeat:=true;
    end else if (LowerCase(Application.GetOptionValue('e', 'repeat')) = 'off') then
    begin
      FOptRepeat:=false;
    end;
  end;
  if Application.HasOption('f', 'fullscreen') then
  begin
    if (LowerCase(Application.GetOptionValue('f', 'fullscreen')) = 'on') then
    begin
      FOptFullscreen:=true;
    end else if (LowerCase(Application.GetOptionValue('f', 'fullscreen')) = 'off') then
    begin
      FOptFullscreen:=false;
    end;
  end;
  if Application.HasOption('t', 'effect') then
  begin
    if (LowerCase(Application.GetOptionValue('t', 'effect')) = 'on') then
    begin
      FOptTransitEffect:=true;
    end else if (LowerCase(Application.GetOptionValue('t', 'effect')) = 'off') then
    begin
      FOptTransitEffect:=false;
    end;
  end;
  if Application.HasOption('n', 'stretchIn') then
  begin
    if (LowerCase(Application.GetOptionValue('n', 'stretchIn')) = 'on') then
    begin
      FOptFit:=true;
    end else if (LowerCase(Application.GetOptionValue('n', 'stretchIn')) = 'off') then
    begin
      FOptFit:=false;
    end;
  end;
  if Application.HasOption('o', 'stretchOut') then
  begin
    if (LowerCase(Application.GetOptionValue('o', 'stretchOut')) = 'on') then
    begin
      FOptExpand:=true;
    end else if (LowerCase(Application.GetOptionValue('o', 'stretchOut')) = 'off') then
    begin
      FOptExpand:=false;
    end;
  end;
  if Application.HasOption('u', 'includSubFolders') then
  begin
    if (LowerCase(Application.GetOptionValue('u', 'includSubFolders')) = 'on') then
    begin
      FOptIncludeSubFolders:=true;
    end else if (LowerCase(Application.GetOptionValue('u', 'includSubFolders')) = 'off') then
    begin
      FOptIncludeSubFolders:=false;
    end;
  end;
  if Application.HasOption('m', 'moniter') then
  begin
    s := trim(Application.GetOptionValue('m', 'moniter'));
    try
      i := strToInt(s);
    except
      i := FOptIntMoniter;
    end;
    if (i > (Screen.MonitorCount-1)) then
    begin
      i := FOptIntMoniter;
    end else
    begin
      if (i > -1) then
      begin
        FOptIntMoniter := i;
      end
    end;
  end;
  // Don't load FOptMinimulFileSizeKiloByte...  If the size is too large, the list will be empty.
  {
  if Application.HasOption('n', 'minimuFileSizeKB') then
  begin
    s := trim(Application.GetOptionValue('n', 'minimuFileSizeKB'));
    try
      i := strToInt(s);
    except
      i := FOptMinimulFileSizeKiloByte;
    end;
    if (i > 0) then
    begin
      // Don't load FOptMinimulFileSizeKiloByte...  If the size is too large, the list will be empty.
      //FOptMinimulFileSizeKiloByte:=i;
    end;
  end;
  }
  if Application.HasOption('p', 'windowPosition') then
  begin
    //TODO:
    //24,24
  end;
  if Application.HasOption('s', 'windowSize') then
  begin
    //TODO:
    //240x380
  end;
  if Application.HasOption('y', 'stayOnTop') then
  begin
    if (LowerCase(Application.GetOptionValue('y', 'stayOnTop')) = 'on') then
    begin
      FoptStayOnTop:=true;
    end else if (LowerCase(Application.GetOptionValue('y', 'stayOnTop')) = 'off') then
    begin
      FoptStayOnTop:=false;
    end;
  end;
  if Application.HasOption('x', 'fileExt') then
  begin
    //TODO:
    //FOptFileExts:='.jpg;.jpeg;.jpe;.png;.gif';
  end;
  if Application.HasOption('l', 'playlistExt') then
  begin
    //TODO:
    //FOptPlaylistExts:='.m3u;.xspf';
    //'.imagelist;.m3u;.m3u8;.xspf'
  end;

  //TODO: some more options to consider.
  // start-at
  // filename-sorting
  // show-on-multiple-displays
  // random-background
  // show-fullpath
  // show-filename

  // Parse other prameters.
  for I := 1 to ParamCount do
  begin
    if (AnsiStartsStr('-',ParamStr(I))) then
    begin
      // Options. has been taken cared of already above.
    end else if (FileExists(ParamStr(I))) then
    begin
      // Found a file
      {$ifdef windows}
      {$else}
      // On UNIX, a directory is also a file.
      if (DirectoryExists(ParamStr(I))) then begin
        // Found a folder
        if not (AnsiStartsStr('.',ExtractFilename(ParamStr(I)))) then
        begin
          FstDirectoryList.Add(ParamStr(I));
        end;
        Continue;
      end;
      {$endif}
      if (FstFileExtList.IndexOf(LowerCase(ExtractFileExt(ParamStr(I)))) >= 0) then
      begin
        // Is a picture file.
        // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
        if not (AnsiStartsStr('.',ExtractFilename(ParamStr(I)))) then
        begin
          f:= FileSize(ParamStr(I));
          // Check file size.
          if f >= (FOptMinimulFileSizeKiloByte) then
          begin
            FstFileList.Add(ParamStr(I));
            //
            FstrInitialSelectedImageFile := ParamStr(I);
          end;
        end;
      end else if (FstPlaylistExtList.IndexOf(LowerCase(ExtractFileExt(ParamStr(I)))) >= 0) then
      begin
        // Found a playlist
        FstPlaylistList.Add(ParamStr(I));
      end;
    {$ifdef windows}
    end else if (DirectoryExists(ParamStr(I))) then
    begin
      // Found a folder.
      // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
      if not (AnsiStartsStr('.',ExtractFilename(ParamStr(I)))) then
      begin
        FstDirectoryList.Add(ParamStr(I));
      end;
    end else if (ParamStr(I) = '*') then
    begin
      // * option. Pretty much the same as "./"
      // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
      if not (AnsiStartsStr('.',ExtractFilename(GetCurrentDir))) then
      begin
        FstDirectoryList.Add(GetCurrentDir);
      end;
    {$endif}
    end;
  end;

  // Search inside folder(s)
  LoadDirectories(FstDirectoryList, FstFileList);

  //TODO: playlist
  // Only if no other files are specified,
  // open the ploylist and add its contents to the filelist.

  if ((FstFileList.Count < 1) and (FstrInitialSelectedImageFile = '')) then
  begin
    // No files are provided in the parameter string, so open "file open" dialog.
    {$ifdef windows}
    if not (DirectoryExists(FstrInitialDir)) then
    begin
         FstrInitialDir := GetWindowsSpecialDir(CSIDL_MYPICTURES);
    end;
    {$endif}
    // Sets default dir
    OpenPictureDialog1.InitialDir:=FstrInitialDir;
    // Sets title
    OpenPictureDialog1.Title:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'')+' - ' + resstrOpenPictures;
    // Open dialog
    if OpenPictureDialog1.Execute then
    begin
      for i:=0 to OpenPictureDialog1.Files.Count -1 do
      begin
        if (FstFileExtList.IndexOf(LowerCase(ExtractFileExt(OpenPictureDialog1.Files[i]))) >= 0) then
        begin
          // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
          if not (AnsiStartsStr('.',ExtractFilename(OpenPictureDialog1.Files[i]))) then
          begin
            f:= FileSize(OpenPictureDialog1.Files[i]);
            // Check file size.
            if f >= (FOptMinimulFileSizeKiloByte) then
            begin
              //FstFileList.Add(OpenPictureDialog1.Files[i]);
              if (i=0) then
              begin
                FstrInitialSelectedImageFile:=OpenPictureDialog1.Files[i];
                // sets init dir for next time.
                FstrInitialDir := ExtractFilePath(OpenPictureDialog1.Files[i]);
              end;
              FstFileList.Add(OpenPictureDialog1.Files[i]);
            end;
          end;
        end else if (FstPlaylistExtList.IndexOf(LowerCase(ExtractFileExt(OpenPictureDialog1.Files[i]))) >= 0) then
        begin
          // playlist.
          FstPlaylistList.Add(OpenPictureDialog1.Files[i]);
        end;
      end;
    end;

    //TODO: playlist
    // open ploylist files and add them to the filelist.
  end;

  // If only one image was selected, add all siblings automatically.
  // "send to" command-line parameters don't accept more than 255.
  //if FstFileList.Count = 1 then
  if ((FstFileList.Count = 1) and (FstrInitialSelectedImageFile <> '')) then
  begin
    LoadSiblings(FstFileList[0], FstFileList);

    //if (FstFileList.count = 0) then
    //   FstFileList.Add(FstrInitialSelectedImageFile);

    // Since automatically added, do not start slideshow at fullscreen later.
    FisSingleFileSelected:=true;
  end;

  // We tried. But there is nothing to do, so self terminate.
  if (FstFileList.Count < 1) then
  begin
    Application.Terminate;
    exit;
  end;

  if FoptBackgroundBlack then
  begin
    self.Color:=clBlack;
    MenuItemBackgroundBlack.Checked:=true;
    MenuItemBackgroundWhite.Checked:=false;
  end else
  begin
    self.color:=clWhite;
    MenuItemBackgroundBlack.Checked:=false;
    MenuItemBackgroundWhite.Checked:=true;
  end;

  {$ifdef windows}
  // System TrayIcon
  TrayIcon1.Visible:=true;
  TrayIcon1.Hint:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');
  //TODO: + moniter[1]    etc
  {$else}
  // icons don't show up nicely on Mac.
  TrayIcon1.Visible:=false;
  {$endif}

  if FOptFullscreen then
  begin
    // Fullscreen. Main form becomes just a background.
    self.ShowInTaskBar:=stNever;
    self.Image1.Visible:=false;
    self.AlphaBlend:=true;
    self.AlphaBlendValue:=1;
    FisStartNormal:=false;
    self.Show;
    self.BringToFront;
  end else
  begin
    // Main form is the viewer.
    self.ShowInTaskBar:=stDefault;
    self.Position:=poDefault;
    self.WindowState:=wsNormal;

    // Just in case, set default;
    self.Top:=120;
    self.Left:=120;
    self.Width:=480;
    self.Height:=480;

    self.AlphaBlend:=true;
    self.AlphaBlendValue:=255;
    self.Image1.Visible:=true;

    if (FstFileList.indexOf(FstrInitialSelectedImageFile) > -1) then
    begin
      FiCurrentFileIndex:=FstFileList.indexof(FstrInitialSelectedImageFile);
    end else
    begin
      // Start with 0
      FiCurrentFileIndex:=0;
    end;

    // Show.
    FisStartNormal := true;
    self.Show;
    self.BringToFront;
    SetForegroundWindow(self.Handle);
  end;
end;

procedure TfrmMain.FormDblClick(Sender: TObject);
begin
  if Fisfullscreen or FisInFrame then
    begin
      if Assigned(frmFullscreen) then
      begin
        // Redirect when form is cripped and received in frmMain.
        if frmFullscreen.Visible then begin
           frmFullscreen.Image1DblClick(Sender);
        end;
      end;
    end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if Application.Terminated then exit;

  if (FOptFullscreen and (not FisFullscreen) and (not FisStartNormal)) then
  begin
    self.PopupMenu:= nil;

    if FOptTransitEffect then
    begin
      self.AlphaBlendValue:=1;
      ShowFullScreen(true);
      // Start transition timer, and timer creates fullscreen
      TimerEffectStart.Enabled:=true;
    end else
    begin
      // Create fullscreen form at "FormActivate" >why was that?
      self.AlphaBlendValue := 1;
      ShowFullScreen(true);
      self.AlphaBlendValue := 255;
      self.BringToFront;
      // SetForegroundWindow(self.Handle);
    end;
  end else if FIsInFrame then
  begin
    //?
  end else
  begin
    // Normal Show for the first time.

    self.PopupMenu:= PopupMenuMain;

    // It must be in "FormShow"
    if fileexists(XMLConfig.FileName) then
    begin
       RestoreFormState;
    end;
    if FoptStayOnTop then SetStayOnTop(true);

    // Show image.
    LoadImage;
  end;

  // Just in case
  if FisFullscreen then
  begin
    if Assigned(frmFullscreen) then
    begin
      frmFullscreen.BringToFront;
      SetForegroundWindow(frmFullscreen.Handle);
    end else
    begin
      // Something went wrong....
      FisFullscreen:=false;
    end;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if Application.Terminated then exit;

  if (FOptFullscreen and (not FisStartNormal)) then
  begin
    if not FOptTransitEffect then
    begin

      frmFullscreen := TfrmFullscreen.create(self);
      frmFullscreen.Color := self.color;
      frmFullscreen.ShowModal;

      // When returned (frmFullscreen is closed), self close.
      close;
    end;
  end;

  // Just in case
  if FisFullscreen then
  begin
    if Assigned(frmFullscreen) then
    begin
      frmFullscreen.BringToFront;
      SetForegroundWindow(frmFullscreen.Handle);
    end else begin
      // Something went wrong....
      FisFullscreen:=false;
    end;
  end;

  //if (self.top < 0) then self.top := 0;
  //if (self.left < -100) then self.left := 0;

end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  {
  if not (FOptFullscreen and FisFullscreen) then
  begin
    // Save normal form size and pos.
    StoreFormState;
  end;
  }
  if FisStartNormal then begin
    StoreFormState;
    // Save options.
    XMLConfig.SetValue('/Opts/Random',FOptRandom);
    XMLConfig.SetValue('/Opts/Repeat',FOptRepeat);
    //XMLConfig.SetValue('/Opts/Fullscreen',FOptFullscreen);
    XMLConfig.SetValue('/Opts/TransitEffect',FOptTransitEffect);
    XMLConfig.SetValue('/Opts/Fit',FOptFit);
    XMLConfig.SetValue('/Opts/Expand',FOptExpand);
    XMLConfig.SetValue('/Opts/Moniter',FOptIntMoniter);
    XMLConfig.SetValue('/Opts/MinimulFileSizeKiloByte',FOptMinimulFileSizeKiloByte);
    XMLConfig.SetValue('/Opts/StayOnTop',FoptStayOnTop);
    XMLConfig.SetValue('/Opts/BackgroundBlack',FoptBackgroundBlack);
    XMLConfig.SetValue('/Opts/IncludeSubFolders',FOptIncludeSubFolders);
    XMLConfig.SetValue('/Opts/FileExts',widestring(FOptFileExts));
    XMLConfig.SetValue('/Opts/PlaylistExts',widestring(FOptPlaylistExts));
    XMLConfig.SetValue('/Opts/IntervalSeconds',FOptIntervalIntSeconds);

    XMLConfig.SetValue('/InitDir/Path',widestring(FstrInitialDir));

  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Clean up
  FstFileExtList.Free;
  FstPlaylistExtList.Free;
  FstFileList.Free;
  FstDirectoryList.Free;
  FstPlaylistList.Free;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var
  i,f,newCurrentIndex:integer;
  FName, strInitialSelectedImageFile:string;
  TmpFileList, TmpDirList, TmpPlayList:TStringList;
  isSingleFSelected:boolean;
begin
  TmpFileList := TStringList.Create;
  TmpDirList := TStringList.Create;
  TmpPlayList := TStringList.Create;
  strInitialSelectedImageFile := '';
  isSingleFSelected:=false;

  for I := Low(FileNames) to High(FileNames) do
  begin
    FName := FileNames[I];
    if (FileExists(FName)) then
    begin
      // Found a file
      {$ifdef windows}
      {$else}
      // On UNIX, a directory is also a file.
      if (DirectoryExists(FName)) then
        Continue;
      {$endif}
      if (FstFileExtList.IndexOf(LowerCase(ExtractFileExt(FName))) >= 0) then
      begin
        // Is a picture file.
        // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
        if not (AnsiStartsStr('.',ExtractFilename(FName))) then
        begin
          f:= FileSize(FName);
          // Check file size.
          if f >= (FOptMinimulFileSizeKiloByte) then
          begin
            TmpFileList.Add(FName);
            //
            strInitialSelectedImageFile := FName;
          end;
        end;
      end else if (FstPlaylistExtList.IndexOf(LowerCase(ExtractFileExt(FName))) >= 0) then
      begin
        // Found a playlist
        TmpPlayList.Add(FName);
      end;
    {$ifdef windows}
    end else if (DirectoryExists(FName)) then
    begin
      // Found a folder.
      // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
      if not (AnsiStartsStr('.',ExtractFilename(FName))) then
      begin
        TmpDirList.Add(FName);
      end;
    {$endif}
    end;
  end;

  LoadDirectories(TmpDirList, TmpFileList);

  if (TmpFileList.Count = 1) and (strInitialSelectedImageFile <> '') then
  begin
    LoadSiblings(TmpFileList[0], TmpFileList);
    isSingleFSelected:=true;
  end;

  if TmpFileList.Count <> 0 then
  begin
    FiCurrentFileIndex:=0;

    fisSingleFileSelected := isSingleFSelected;
    fstrInitialSelectedImageFile := strInitialSelectedImageFile;
    FstFileList.Assign(TmpFileList);
    FstDirectoryList.Assign(TmpDirList);
    FstPlaylistList.Assign(TmpPlayList);

    if (fstrInitialSelectedImageFile <> '') then
    begin
      newCurrentIndex:=FstFileList.indexOf(FstrInitialSelectedImageFile);
      if (newCurrentIndex > -1) then
      begin
        FiCurrentFileIndex:=newCurrentIndex;
      end else
      begin
        FiCurrentFileIndex:=0;
      end;
    end;

    LoadImage;
    SetFocus;
    BringToFront;
  end;

  TmpFileList.Free;
  TmpDirList.Free;
  TmpPlayList.Free;
end;

procedure TfrmMain.TimerEffectStartTimer(Sender: TObject);
begin
  // While doing this. A user might click the main form ....
  // If done so, popup won't work on fullscreen..
  // -> Don't set popup menu in main form or disable popup temporarily.

  // Disable main form popup.
  self.PopupMenu:= nil;

  if self.AlphaBlendValue < 255 then
  begin
    if (self.AlphaBlendValue < 150 ) then begin
       self.AlphaBlendValue := self.AlphaBlendValue + 10;
    end else
    begin
      if (self.AlphaBlendValue + 15 >= 255) then begin
        self.AlphaBlendValue := 255;
      end else begin
        self.AlphaBlendValue := self.AlphaBlendValue + 15;
      end;
    end;
  end else begin
    TimerEffectStart.Enabled:=false;

    frmFullscreen := TfrmFullscreen.create(self);
    frmFullScreen.StartWith:=FiCurrentFileIndex;
    frmFullscreen.Color := self.color;
    // Show full screen.
    frmFullscreen.ShowModal;

    //if (self.top < 0) then self.top := 0;
    //if (self.left < -100) then self.left := 0;

    // Returned from fullscreen
    if FisStartNormal then
    begin
      // If start with normal then, do not close.
      // DoneFullscreen will be called

      Screen.Cursor:=crDefault;
      Screen.UpdateScreen;
      Image1.Visible:=true;
      ShowFullScreen(false);
      // We are back normal;
      FOptFullscreen:=false;
    end else
    begin
      // We start with fullscreen, so close after fullscreen.
      close;
    end;

  end;
end;

procedure TfrmMain.TrayIcon1DblClick(Sender: TObject);
begin
  // Restore form if minimized or hide if non-minimized.
  if (not FisFullscreen) then
  begin
    if (self.WindowState = wsMinimized) then
    begin
      self.WindowState := wsNormal;
      self.Show;
      self.BringToFront;
      SetForegroundWindow(self.Handle);
    end else if (self.WindowState = wsNormal) then
    begin
      WindowState:=wsMinimized;
      Hide;
    end;
  end;
end;

function TfrmMain.DispayImage(filename:string):boolean;
begin
  try
    Image1.Picture.LoadFromFile(filename);
    Image1Resize(nil);
    result:=true;
  except
    on E: Exception do
    begin
      // 'FPImageException'
      Image1.Picture.Clear;
      with image1.Canvas do
        begin
          Brush.Style := bsClear;
          Font.Color := clWhite;
          TextOut(24,24, 'File load error: ' + E.ClassName +' - '+ E.Message );
          TextOut(24,52, 'File: ' + filename);
      end;
      Image1.Update;
      result:=false;
    end;
  end;
end;

procedure TfrmMain.LoadImage;
var
  strPath:string;
begin
  if FileList.Count > 0 then
  begin

    strPath := MinimizeName(FileList[FiCurrentFileIndex],Self.Canvas, self.width - 300);

    if (FileList.Count = 1) then
    begin
      //Self.Caption:=FileList[FiCurrentFileIndex];
      Self.Caption:=strPath;
    end else
    begin
      //Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
      Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + strPath;
    end;

    DispayImage(FileList[FiCurrentFileIndex]);
  end;
end;

procedure TfrmMain.Image1Resize(Sender: TObject);
var
  curWidth,curHeight:integer;
begin
  if not Assigned(Image1.Picture) then exit;

  Image1.Stretch:=false;
  curWidth := self.ClientWidth;
  curHeight:= self.ClientHeight;

    if FOptFit then
    begin
      // Fit only when larger than screen size.
      if ((Image1.Picture.Width > curWidth) or
              (Image1.Picture.height > curHeight)) then
      begin
        Image1.Stretch:=true;
        Image1.StretchInEnabled:=true;
        Image1.StretchOutEnabled:=true;
        Image1.AntialiasingMode:=amOn;
      end else
      begin
        Image1.Stretch:=false;
        Image1.StretchInEnabled:=false;
        Image1.StretchOutEnabled:=false;
        Image1.AntialiasingMode:=amOff;
      end;
    end else
    begin
      Image1.Stretch:=false;
      Image1.StretchInEnabled:=false;
      Image1.StretchOutEnabled:=false;
      Image1.AntialiasingMode:=amOff;
    end;

    if FOptExpand then
    begin
      if ((Image1.Picture.Width < curWidth) and
              (Image1.Picture.height < curHeight)) then
      begin
        Image1.Stretch:=true;
        Image1.StretchOutEnabled:=true;
        Image1.StretchInEnabled:=true;
        Image1.AntialiasingMode:=amOn;
      end;
    end;
end;

procedure TfrmMain.MenuItemAboutClick(Sender: TObject);
begin
  MenuItemSysAboutClick(Sender);
end;

procedure TfrmMain.MenuItemBackgroundBlackClick(Sender: TObject);
begin

  FoptBackgroundBlack:=true;
  self.Color:=clBlack;
  MenuItemBackgroundBlack.Checked:=true;
  MenuItemBackgroundWhite.Checked:=false;

end;

procedure TfrmMain.MenuItemBackgroundWhiteClick(Sender: TObject);
begin
                                        
  FoptBackgroundBlack:=false;
  self.Color:=clWhite;
  MenuItemBackgroundBlack.Checked:=false;
  MenuItemBackgroundWhite.Checked:=true;

end;

procedure TfrmMain.Image1DblClick(Sender: TObject);
begin
    if Fisfullscreen or FisInFrame then
    begin
      if Assigned(frmFullscreen) then
      begin
        // Redirect when form is cripped and received in frmMain.... but not working here because Image1 is hidden.
        if frmFullscreen.Visible then begin
           frmFullscreen.Image1DblClick(Sender);
        end;
      end;
    end else
    begin
      //
      MenuItemSlideshowClick(nil);
    end;
end;

procedure TfrmMain.MenuItemSlideshowClick(Sender: TObject);
begin
  // Start fullscreen
  if (not FisFullscreen) and (not FisInFrame) then
  begin
    FOptFullscreen:=true;
    self.PopupMenu:= nil;

    // This is telling TimerEffectStart to go back to normal when done.
    FisStartNormal:=true;

    Image1.Visible:=false;

    // Sets current screen of the form.
    FCurrentMonitor:=Screen.MonitorFromWindow(handle);
    FOptIntMoniter:=getCurrentMonitorIndex();

    if FOptTransitEffect then
    begin
      //TODO: form dissapears at once. maybe fadeout timer?
      self.AlphaBlendValue:=1;
      ShowFullScreen(true);

      // Start transition timer, and timer creates fullscreen
      TimerEffectStart.Enabled:=true;
    end else
    begin

      ShowFullScreen(true);

      frmFullscreen := TfrmFullscreen.create(self);
      frmFullScreen.StartWith:=FiCurrentFileIndex;
      frmFullscreen.Color := self.color;
      frmFullscreen.ShowModal;

      // DoneFullscreen will be called here.

      Image1.Visible:=true;
      ShowFullScreen(false);
      Screen.Cursor:=crDefault;
      //if (self.top < 0) then self.top := 0;
      //if (self.left < -100) then self.left := 0;
    end;

  end;
end;

procedure TfrmMain.Image1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
    currentShiftState: TShiftState;
  strPath:string;
begin
  if (not FisFullscreen) and (not FisInFrame) then
  begin
    // check if Ctrl is pressed
    currentShiftState:=GetKeyShiftState;
    if ssCtrl in currentShiftState then
    begin
    // Do nothing. Ctrl key down should trigger a zooming in the ScrollBox.

    end else
    begin
      if FiCurrentFileIndex < FileList.Count -1 then
      begin
       screen.Cursor:=crHourGlass;
       DispayImage(FileList[FiCurrentFileIndex+1]);
       FiCurrentFileIndex:=FiCurrentFileIndex+1;
       //Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
       strPath := MinimizeName(FileList[FiCurrentFileIndex],Self.Canvas, self.width - 300);
       Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + strPath;
       screen.Cursor:=crDefault;
       //Handled:=true;
      end;
    end;
  end;
end;

procedure TfrmMain.Image1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  currentShiftState: TShiftState;
  strPath:string;
begin
  if (not FisFullscreen) and (not FisInFrame) then
  begin
    // check if Ctrl is pressed
    currentShiftState:=GetKeyShiftState;
    if ssCtrl in currentShiftState then
    begin
    // Do nothing. Ctrl key down should trigger a zooming in the ScrollBox.

    end else
    begin
      // go previouse image
      if FiCurrentFileIndex > 0 then
      begin
        screen.Cursor:=crHourGlass;
        DispayImage(FileList[FiCurrentFileIndex-1]);
        FiCurrentFileIndex:=FiCurrentFileIndex-1;
        //Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
        strPath := MinimizeName(FileList[FiCurrentFileIndex],Self.Canvas, self.width - 300);
        Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + strPath;
        screen.Cursor:=crDefault;
        //Handled:=true;
      end;
    end;
  end;
end;

procedure TfrmMain.ScrollBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if FintZoomingScaleFactor < 50 then begin
     FintZoomingScaleFactor:=FintZoomingScaleFactor+1;
     DrawImage;
  end;
end;

procedure TfrmMain.ScrollBox1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if FintZoomingScaleFactor > 1 then begin
     FintZoomingScaleFactor:=FintZoomingScaleFactor-1;
     DrawImage;
  end;
end;

procedure TfrmMain.DrawImage;
var
  ImageRect: TRect;
begin
  //ImageRect := Rect(0,0,(Image1.Picture.Width * FintZoomingScaleFactor),(Image1.Picture.Height * FintZoomingScaleFactor));
  SetRect(ImageRect,0,0,(Image1.Picture.Width * FintZoomingScaleFactor),(Image1.Picture.Height * FintZoomingScaleFactor));

  PaintBox1.Width := ImageRect.Right;
  PaintBox1.Height := ImageRect.Bottom;
  PaintBox1.Canvas.StretchDraw(ImageRect, image1.Picture.Bitmap);
end;

procedure TfrmMain.PaintBox1Paint(Sender: TObject);
begin
  DrawImage;
end;

procedure TfrmMain.MenuItemNextClick(Sender: TObject);
var
  strPath:string;
begin
  if FiCurrentFileIndex < FileList.Count -1 then
  begin
    screen.Cursor:=crHourGlass;
    DispayImage(FileList[FiCurrentFileIndex+1]);
    FiCurrentFileIndex:=FiCurrentFileIndex+1;
    strPath := MinimizeName(FileList[FiCurrentFileIndex],Self.Canvas, self.width - 300);
    Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + strPath;
    screen.Cursor:=crDefault;
  end;
end;

procedure TfrmMain.MenuItemBackClick(Sender: TObject);
var
  strPath:string;
begin
  if FiCurrentFileIndex > 0 then
  begin
    screen.Cursor:=crHourGlass;
    DispayImage(FileList[FiCurrentFileIndex-1]);
    FiCurrentFileIndex:=FiCurrentFileIndex-1;
    strPath := MinimizeName(FileList[FiCurrentFileIndex],Self.Canvas, self.width - 300);
    Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + strPath;
    screen.Cursor:=crDefault;
  end;
end;

procedure TfrmMain.MenuItemQuitClick(Sender: TObject);
begin
  close;
end;

procedure TfrmMain.DoneFullscreen(strCurr:string);
var
  i:integer;
begin
  if FIsStartNormal then
  begin
    // Restore main form popup.
    self.PopupMenu:= PopupMenuMain;

    // Must be here. Otherwise one pic fullscreen hangs.
    if FileList.Count <= 1 then exit;
    if strCurr <> '' then
    begin
      i:=FileList.IndexOf(strCurr);
      if (i > -1) then
      begin
        FiCurrentFileIndex:=i;
        LoadImage();
      end;
    end;
    Screen.Cursor:=crDefault; //again
  end;

  //if (self.top < 0) then self.top := 0;
  //if (self.left < -100) then self.left := 0;
end;

procedure TfrmMain.MenuItemSlideshowInFrameClick(Sender: TObject);
{$ifdef windows}
var
  titlebarheight:integer;
  //rgn:Cardinal;
{$else}
var
  Form: TForm;
{$endif}
begin
  if Fisfullscreen then exit;
  //if FileList.Count <= 1 then exit;

  if FisInFrame then
  begin
    //close it
    if Assigned(frmFullscreen) then
    begin
      if frmFullscreen.Visible then
      begin
        frmFullscreen.Close;
      end;
    end;
  end else
  begin
    // Disable main form popup.
    self.PopupMenu:= nil;

    {$ifdef windows}
    FOrigBounds:= BoundsRect;
    self.BorderStyle:=bsNone;
    BoundsRect := FOrigBounds;

    self.left := self.left + GetSystemMetrics(SM_CYFRAME); // added in v1.2.18
    titlebarheight:=GetSystemMetrics(SM_CYCAPTION)+ GetSystemMetrics(SM_CYFRAME);
    self.height := self.height + titlebarheight;

    // Rounded corner window on per with Windows11.
    //rgn:=CreateRoundRectRgn(0,0,self.width,self.height,16,16);
    //SetWindowRgn(Handle,Rgn,True);

    // Could be better?
    DoubleBuffered := True;
    EnableBlur;

    {$else}
      // https://forum.lazarus.freepascal.org/index.php?topic=38675.0
      FOrigBounds:= BoundsRect;
      Hide;
      self.BorderStyle:=bsNone;
      Show;

      Form := TForm.Create(nil);
      try
        Parent := Form;
        Parent := nil;
      finally
        Form.Free;
      end;

      BoundsRect := FOrigBounds;
    {$endif}

    FisInFrame:=true;
    self.Caption:='InFrame Slideshow';
    Image1.Visible:=false;
    frmFullscreen := TfrmFullscreen.create(self);
    frmFullScreen.StartWith:=FiCurrentFileIndex;
    frmFullscreen.Color := self.color;
    frmFullscreen.Parent := self;
    // Set main form popup.
    self.PopupMenu:= frmFullscreen.PopupMenu;
    frmFullscreen.Visible:=true;
    frmFullscreen.Show;

    // inFrame you have to
    // Call frmFullscreen.FormResize(self);  when resize.
    // Call DoneInFrame(); from frmFullscreen when close fullscreen.
  end;
end;

procedure TfrmMain.DoneInFrame(strCurr :string);
var
  {$ifdef windows}
  {$else}
  //Form: TForm;
  {$endif}
  i:integer;
begin
  FisInFrame:=false;
  // Restore main form popup.
  self.PopupMenu:= PopupMenuMain;

  {$ifdef windows}
  BoundsRect:= FOrigBounds;
  self.BorderStyle:=bsSizeable;
  BoundsRect:=FOrigBounds;
  //if (self.top < 0) then self.top := 0;
  //if (self.left < -100) then self.left := 0;

  {$else}
    // https://forum.lazarus.freepascal.org/index.php?topic=38675.0
    self.BorderStyle:=bsSizeable;

    //Form := TForm.Create(nil);
    //try
    //  Parent := Form;
    //  Parent := nil;
    //finally
    //  Form.Free;
    //end;

    BoundsRect:= FOrigBounds;
  {$endif}

  Image1.BringToFront;
  Image1.Visible:=true;

  if FileList.Count <= 1 then exit;
  if strCurr <> '' then
  begin
    i:=FileList.IndexOf(strCurr);
    if (i > -1) then
    begin
      FiCurrentFileIndex:=i;
      LoadImage();
    end;
  end;

  Image1.Repaint;
  Image1.Refresh;
  Image1.BringToFront;
  screen.Cursor:=crDefault;
  //if (self.top < 0) then self.top := 0;
  //if (self.left < -100) then self.left := 0;
end;

procedure TfrmMain.FormResize(Sender: TObject);
var
  strPath:string;
begin
  if FisInFrame then
  begin
    if assigned(frmFullscreen) and frmFullscreen.Visible then
    begin
      frmFullscreen.FormResize(self);
    end;
  end else
  begin
    if FileList.Count > 0 then
    begin

      strPath := MinimizeName(FileList[FiCurrentFileIndex],Self.Canvas, self.width - 300);

      if (FileList.Count = 1) then
      begin
        Self.Caption:=strPath;
      end else
      begin
        Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + strPath;
      end;

    end;
  end;
end;

procedure TfrmMain.SetCaption(strCaption:string);
begin
  Self.Caption:= strCaption;
end;

procedure TfrmMain.SetStayOnTop(bln:Boolean);
var
  BeforeBounds : TRect;
begin
  if bln then
  begin
    self.FormStyle:=fsSystemStayOnTop;
    MenuItemStayOnTop.Checked:=true;
  end else
  begin
    {$ifdef windows}
    if FisInFrame then
    begin
      // "FormStyle:=fsNormal" causes window pos to move to 0,0 so..
      BeforeBounds:= BoundsRect;
    end;
    {$endif}

    self.FormStyle:=fsNormal;
    MenuItemStayOnTop.Checked:=false;

    {$ifdef windows}
    if FisInFrame then
    begin
      self.BorderStyle:=bsNone;
      // Blur again
      DoubleBuffered := True;
      EnableBlur;
      // re-set position.
      BoundsRect := BeforeBounds;
    end;
    {$endif}
  end;
  self.FoptStayOnTop:=bln;
end;

procedure TfrmMain.ApplicationProperties1Exception(Sender: TObject; E: Exception);
begin
  // We don't want to show dialog while showing fullscreen with Modal window.
  // e.g. message dialog showing behind the modal window and no way to click or see.
  // so let's just terminate or .. ignore.
  // TODO: error logging.

  //Application.Terminate;
  //Halt(1);

end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FisInFrame then
  begin
    if Assigned(frmFullscreen) then
    begin
      if frmFullscreen.Visible then
      begin

        if ((Key = VK_F11) or (Key = VK_ESCAPE)) then
        begin
          // Close;
          frmFullscreen.Close;
        end;

        {
        // Pass it to fullscreen.
        if ((Key = VK_F11) or (Key = VK_ESCAPE) or (Chr(Key) = 'F') or (Chr(Key) = 'S')) then
        begin
          // Close;
          frmFullscreen.Close;
        end;

        // Next
        if (Key = VK_RIGHT) then
        begin
          frmFullscreen.PlaybackNext(sender);
        end;
        // Back
        if (Key = VK_LEFT) or (Key = VK_BACK) then
        begin
          frmFullscreen.PlaybackBack(sender);
        end;
        // Pause/Start
        if (Key = VK_PAUSE) or (Key = VK_SPACE) then
        begin
          frmFullscreen.PlaybackPause(sender);
        end;
        //TODO: inFrame: frmMain steals fullscreen's shortcut keys. so.
        if (Chr(Key) = 'I') then
        begin
          // Stretch In
          frmFullscreen.MenuItemFitClick(sender);
        end;
        if (Chr(Key) = 'O') then
        begin
          // Stretch Out
          frmFullscreen.MenuItemExpandClick(sender);
        end;
        if (Chr(Key) = 'E') then
        begin
          // Effect
          frmFullscreen.MenuItemEffectClick(sender);
        end;
        if (Chr(Key) = 'N') then
        begin
          // Random
          frmFullscreen.MenuItemRandomClick(sender);
        end;
        if (Chr(Key) = 'R') then
        begin
          // Repeat
          frmFullscreen.MenuItemRepeatClick(sender);
        end;

        if ((Key = VK_RMENU) or (Key = VK_LMENU) or (Key = VK_MENU)) then
        begin
          frmFullscreen.PopupMenu.PopUp(0,0);
        end;
        }
      end;

    end;
  end else if FOptFullscreen and FisFullscreen then
  begin
    if ((Key = VK_F11) or (Key = VK_ESCAPE)) then
    begin
      if FisStartNormal then begin
        if Assigned(frmFullscreen) then
          if frmFullscreen.Visible then
            frmFullscreen.Close;
      end else
      begin
        self.close;
      end;
    end;
  end else begin
    // Normal image view.

    if (Chr(Key) = 'Q') then
    begin
      if (ssCtrl in Shift) then
      begin
        close;
      end;
    end else
    if ((Key = VK_F11)) then
    begin
      // Fullscreen slideshow
      MenuItemSlideshowClick(nil);
    end else
    begin
      if (ssCtrl in Shift) then
      begin
        // picture zoom
        ScrollBox1.Visible := true;
      end;
    end;
    {
    // Since form steals Image menu... and we have the problem assigning menu for form.

    // Fullscreen slideshow
    if ((Key = VK_F11) or (Chr(Key) = 'F')) then
    begin
     MenuItemSlideshowClick(nil);
    end;

    // InFrame slideshow
    if (Chr(Key) = 'S') then
    begin
     MenuItemSlideshowInFrameClick(nil);
    end;

    // Next
    if (Key = VK_RIGHT) then begin
      MenuItemNextClick(nil);
    end;
    // Back
    if (Key = VK_LEFT) or (Key = VK_BACK) then
    begin
      MenuItemBackClick(nil);
    end;
    // Stretch in
    if (Chr(Key) = 'I') then
    begin
      MenuItemStretchInClick(nil);
    end;
    // Stretch out
    if (Chr(Key) = 'O') then
    begin
      MenuItemStretchOutClick(nil);
    end;

    if ((Key = VK_RMENU) or (Key = VK_LMENU) or (Key = VK_MENU)) then
    begin
      //Image1.PopupMenu.PopUp(self.top,self.left);
      //self.PopupMenu.PopUp(self.top,self.left);
    end;
    }
  end;
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (not (ssCtrl in Shift)) then
  begin
    // picture zoom
    ScrollBox1.Visible := false;
    FintZoomingScaleFactor := 1;
  end;
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FisInFrame then
  begin
    if Assigned(frmFullscreen) then
    begin
      // Redirect when form is cripped and receive inputs while slideshow.
      if frmFullscreen.Visible then begin
         frmFullscreen.Image1MouseDown(Sender,Button,Shift,X,Y);
      end;
    end;
  end;
end;

procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FisInFrame then
  begin
    if Assigned(frmFullscreen) then
    begin
      // Redirect when form is cripped and receive inputs while slideshow.
      if frmFullscreen.Visible then begin
         frmFullscreen.Image1MouseMove(Sender,Shift,X,Y);
      end;
    end;
  end;
end;

procedure TfrmMain.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FisInFrame then
  begin
    if Assigned(frmFullscreen) then
    begin
      // Redirect when form is cripped and receive inputs while slideshow.
      if frmFullscreen.Visible then begin
         frmFullscreen.Image1MouseUp(Sender,Button,Shift,X,Y);
      end;
    end;
  end;
end;

procedure TfrmMain.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Fisfullscreen or FisInFrame then
  begin
    if Assigned(frmFullscreen) then
    begin
      // Redirect when form is cripped and receive inputs while slideshow.
      if frmFullscreen.Visible then begin
         frmFullscreen.FormMouseWheelDown(Sender,Shift,MousePos,Handled);
      end;
    end;
  end;
end;

procedure TfrmMain.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Fisfullscreen or FisInFrame then
  begin
    if Assigned(frmFullscreen) then
    begin
      // Redirect when form is cripped and receive inputs while slideshow.
      if frmFullscreen.Visible then begin
         frmFullscreen.FormMouseWheelUp(Sender,Shift,MousePos,Handled);
      end;
    end;
  end;
end;

procedure TfrmMain.MenuItemStayOnTopClick(Sender: TObject);
begin
  if (self.FormStyle = fsSystemStayOnTop) then
  begin
    SetStayOnTop(false);
  end else
  begin
    SetStayOnTop(true);
  end;
end;

procedure TfrmMain.MenuItemStretchInClick(Sender: TObject);
begin
  if FOptFit then FOptFit:=false else FOptFit:=true;
  Image1Resize(self);
end;

procedure TfrmMain.MenuItemStretchOutClick(Sender: TObject);
begin
  if FOptExpand then FOptExpand:=false else FOptExpand:=true;
  Image1Resize(self);
end;

procedure TfrmMain.MenuItemSysAboutClick(Sender: TObject);
begin
  if (not FisFullscreen) or (not FisInFrame) then
  begin
    frmAbout := TfrmAbout.Create(self);
    frmAbout.Caption:=' '+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');

    frmAbout.StaticTextAppsVer.Caption := 'Image Viewer' + ' - ' + FstrAppVer;
    frmAbout.StaticTextWho.Caption := 'by torum';
    frmAbout.StaticTextWebSite.Caption:='https://torum.github.io/Image-viewer/';

    frmAbout.ShowModal;
    // No need to free. closeAction caFree.
  end;
end;

procedure TfrmMain.MenuItemSysQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.PopupMenuMainPopup(Sender: TObject);
begin
  if (FstFileList.Count > 1) then
  begin
    if FisSingleFileSelected then
    begin
      MenuItemSlideshowInFrame.Caption:=resstrInFrameView;
      MenuItemSlideshow.Caption:=resstrFullscreenView;
    end else
    begin
      MenuItemSlideshowInFrame.Caption:=resstrStartInFrameSlideshow ;
      MenuItemSlideshow.Caption:=resstrStartFullscreenSlideshow ;
    end;
  end else
  begin
    MenuItemSlideshowInFrame.Caption:=resstrInFrameView ;
    MenuItemSlideshow.Caption:=resstrFullscreenView;

    MenuItemNext.enabled:=false; //visible or enabled:=false;
    MenuItemBack.enabled:=false;
  end;

  if FOptFit then MenuItemStretchIn.Checked:=true else MenuItemStretchIn.Checked:=false;
  if FOptExpand then MenuItemStretchOut.Checked:=true else MenuItemStretchOut.Checked:=false;

end;

procedure TfrmMain.PopupMenuSystemPopup(Sender: TObject);
begin
  if (FisFullscreen) or (FisInFrame) then
  begin
    MenuItemSysAbout.Enabled:=false;
    MenuItemSysAbout.Visible:=false;
    MenuItemSysBo.Visible:=false;
  end else
  begin
    MenuItemSysAbout.Enabled:=true;
    MenuItemSysAbout.Visible:=true;
    MenuItemSysBo.Visible:=true;
  end;
end;

procedure TfrmMain.SetMoniter(intMoniter:integer);
begin
  if ((Screen.MonitorCount-1) >= intMoniter) and (intMoniter >= 0) then
  begin
    FOptIntMoniter:=intMoniter;

    if FOptFullscreen and FisFullScreen and Assigned(frmFullscreen) then
    begin
        ShowFullScreen(true);
    end else
    begin
      // non fullscreen.
    end;
  end;
end;

function TfrmMain.GetCurrentMonitor():TMonitor;
begin
  if not Assigned(FCurrentMonitor) then
  begin
    FCurrentMonitor := Screen.MonitorFromWindow(Handle);
    result:=FCurrentMonitor;
  end else
  begin
    result:=FCurrentMonitor;
  end;
end;

function TfrmMain.GetCurrentMonitorIndex():integer;
var
  i:integer;
begin
  result:=0;
  for i:=0 to Screen.MonitorCount-1 do
  begin
    if CurrentMonitor = Screen.Monitors[i] then
    begin
      result:=i;
      break;
    end;
  end;
end;

procedure TfrmMain.ShowFullScreen(blnOn: boolean);
begin
  {$ifdef windows}
  SetFullScreen_Win32(blnOn);
  {$else}
  SetFullScreen_Universal(blnOn);
  {$endif}
  FisFullscreen:=blnOn;
end;

procedure TfrmMain.SetFullScreen_Universal(blnOn: boolean);
{$ifdef darwin}var
  Form: TForm;{$endif}
begin
  if blnOn then
  begin
    if not FisFullScreen then
    begin
      // Save original windowstate
      FOrigWndState:= WindowState;
      FOrigBounds := BoundsRect;
    end;
    // Don't do this if the form is modal on linux. And it won't work with multi moniters.
    // WindowState:=wsFullScreen;
    {$ifdef windows}
    // Don't do this at runtime on linux!
    // https://forum.lazarus.freepascal.org/index.php?topic=38675.0
    BorderStyle:= bsNone;

    {$endif}

    {$ifdef darwin}
    // Hide title bar
    // https://forum.lazarus.freepascal.org/index.php?topic=38675.0
    self.BorderStyle:=bsNone;
    Form := TForm.Create(nil);
    try
      Parent := Form;
      Parent := nil;
    finally
      Form.Free;
    end;
    {$endif}

    if (CurrentMonitor <> Screen.Monitors[FOptIntMoniter]) then
    begin
      BoundsRect:= Screen.Monitors[FOptIntMoniter].BoundsRect;
    end else
    begin
      BoundsRect:= CurrentMonitor.BoundsRect;
    end;

    {$ifdef darwin}
    // on Mac, don't call SW_SHOWFULLSCREEN on main. And make sure to call SW_SHOWNORMAL on close window on Mac.
    {$else}
    ShowWindow(Handle, SW_SHOWFULLSCREEN);
    {$endif}

  end else
  begin
    WindowState:= FOrigWndState;
    {$ifdef windows}
    // Don't do this at runtime on linux!
    BorderStyle:= bsSizeable;
    {$endif}

    {$ifdef darwin}
    // Un-hide title bar
    // https://forum.lazarus.freepascal.org/index.php?topic=38675.0
    self.BorderStyle:=bsSizeable;
    Form := TForm.Create(nil);
    try
      Parent := Form;
      Parent := nil;
    finally
      Form.Free;
    end;
    {$endif}

    {$ifdef darwin}
    // on Mac, don't call SW_SHOWFULLSCREEN on main. And make sure to call SW_SHOWNORMAL on close window on Mac.
    {$else}
    ShowWindow(Handle, SW_SHOWNORMAL);
    {$endif}
    BoundsRect:= FOrigBounds;
  end;
end;

procedure TfrmMain.SetFullScreen_Win32(blnOn: boolean);
begin
  if blnOn then
  begin
    if not FisFullScreen then
    begin
      FOrigWndState:= WindowState;
      FOrigBounds:= BoundsRect;
      // Must be this order
      WindowState:=wsFullScreen; //1
      BorderStyle:= bsNone;      //2
    end;

    if (CurrentMonitor <> Screen.Monitors[FOptIntMoniter]) then
    begin
      BoundsRect:= Screen.Monitors[FOptIntMoniter].BoundsRect;
    end else
    begin
      BoundsRect:= CurrentMonitor.BoundsRect;
    end;

    {$ifdef windows}
    // Not really good?
    //DoubleBuffered := True;
    //EnableBlur;
    {$endif}

    // ShowWindow(Handle, SW_SHOWFULLSCREEN);
  end else
  begin
    WindowState:= FOrigWndState;
    BoundsRect:= FOrigBounds;
    BorderStyle:= bsSizeable;

    // ShowWindow(Handle, SW_SHOWNORMAL);
    BoundsRect:= FOrigBounds;
  end;
end;

procedure TfrmMain.RestoreFormState;
var
  LastWindowState: TWindowState;
begin

  with XMLConfig do
  begin
    LastWindowState := TWindowState(GetValue('WindowState', Integer(WindowState)));

    if LastWindowState = wsMaximized then
    begin
      WindowState := wsNormal;
      BoundsRect := Bounds(
        GetValue('RestoredLeft', RestoredLeft),
        GetValue('RestoredTop', RestoredTop),
        GetValue('RestoredWidth', RestoredWidth),
        GetValue('RestoredHeight', RestoredHeight));
      WindowState := wsMaximized;
    end else
    begin
      WindowState := wsNormal;
      BoundsRect := Bounds(
        GetValue('NormalLeft', Left),
        GetValue('NormalTop', Top),
        GetValue('NormalWidth', Width),
        GetValue('NormalHeight', Height));

        //if (self.top < 0) then self.top := 0;
        //if (self.left < -100) then self.left := 0;
    end;
  end;

end;

procedure TfrmMain.StoreFormState;
begin

  // Save form state.
  with XMLConfig do
  begin
    SetValue('NormalLeft', Left);
    SetValue('NormalTop', Top);
    SetValue('NormalWidth', Width);
    SetValue('NormalHeight', Height);

    SetValue('RestoredLeft', RestoredLeft);
    SetValue('RestoredTop', RestoredTop);
    SetValue('RestoredWidth', RestoredWidth);
    SetValue('RestoredHeight', RestoredHeight);

    SetValue('WindowState', Integer(WindowState));
  end;


end;

{$ifdef windows}
// requires delphi mode.
// https://wiki.lazarus.freepascal.org/Aero_Glass
procedure TfrmMain.EnableBlur;
const
  WCA_ACCENT_POLICY = 19;
  ACCENT_ENABLE_BLURBEHIND = 3;
  ACCENT_ENABLE_ACRYLICBLURBEHIND = 4;
  DrawLeftBorder = $20;
  DrawTopBorder = $40;
  DrawRightBorder = $80;
  DrawBottomBorder = $100;
var
  dwm10: THandle;
  data : TWinCompAttrData;
  accent: AccentPolicy;
begin

  dwm10 := LoadLibrary('user32.dll');
  try
    @SetWindowCompositionAttribute := GetProcAddress(dwm10, 'SetWindowCompositionAttribute');
    if @SetWindowCompositionAttribute <> nil then
    begin
      accent.AccentState := ACCENT_ENABLE_BLURBEHIND;
      //accent.AccentState := ACCENT_ENABLE_ACRYLICBLURBEHIND;
      //accent.GradientColor := (100 SHL 24) or ($00E3E0DE);
      accent.GradientColor := ($000000FF);

      accent.AccentFlags := DrawLeftBorder or DrawTopBorder or DrawRightBorder or DrawBottomBorder;

      data.Attribute := WCA_ACCENT_POLICY;
      data.dataSize := SizeOf(accent);
      data.pData := @accent;
      SetWindowCompositionAttribute(self.Handle, data);

    end
    else
    begin
      //ShowMessage('Not found Windows 10 blur API');
    end;
  finally
    FreeLibrary(dwm10);
  end;

end;
(*
procedure TfrmMain.AeroGlass;
var
  Aero: BOOL;
  Area: TRect;
  hDWM: THandle;
begin
  hDWM:=LoadLibrary('dwmapi.dll');
  try
    @DwmIsCompositionEnabled:=GetProcAddress(hDWM,'DwmIsCompositionEnabled');
    if @DwmIsCompositionEnabled<>nil then
        DwmIsCompositionEnabled(Aero);
    if Aero then
    begin
      Area:=Rect(-1,-1,-1,-1);
      Color:=clBlack;
      @DwmExtendFrameIntoClientArea:=GetProcAddress(hDWM,'DwmExtendFrameIntoClientArea');
      if @DwmExtendFrameIntoClientArea<>nil then
          DwmExtendFrameIntoClientArea(Handle,@Area);

    end
    else ShowMessage('Aero is Disabled');
  finally
    FreeLibrary(hDWM);
  end;
end;
*)

initialization
  SetWindowCompositionAttribute := GetProcAddress(GetModuleHandle(user32), 'SetWindowCompositionAttribute');
{$endif}
end.

























