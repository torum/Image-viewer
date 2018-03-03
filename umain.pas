unit UMain;

{$mode objfpc}{$H+}

{
Source:
 https://github.com/torumyax/Image-viewer

No extra components required.

Compiled and tested on
 Windows 10 (64bit): Lazarus 1.8.0 r56594 FPC 3.0.4 x86_64-win64-win32/win64
 Ubuntu 17.10 (64bit): Lazarus 1.8.0 rc4+dfsg-1 FPC 3.0.2 x86_64-linux-gtk2
 Ubuntu 16.04 LTS (64bit): Lazarus 1.9.0 trunk, FPC 3.0.4
 macOS 10.13.3 (64bit) High Sierra: Lazarus 1.8.0 rexported FPC 3.0.4 i386-darwin-carbon
 macOS 10.11.6 (64bit) El Capitan: Lazarus 1.9.0 carbon trunk, FPC 3.0.4

TODO:
 - priority 1 -
 i18n
 UWP packaging.
 - priority 2 -
 load playlist.
 - priority 3 -
 more Command line options.
 file drop handling(win).
 preLoading image for slideshow.
 OS's power-save event aware.
 Modal dialog with options and playlist edit tab (drag & drop files).

Known issues and bugs:
 On Windows, PNG (depth 24) antialising isn't working when stretch.
  https://forum.lazarus.freepascal.org/index.php?topic=24408.0
  http://forum.lazarus.freepascal.org/index.php?topic=19542.0
 On Windows, inFrame "window" does not have shaddow.
 On Ubuntu, inFrame transit effect doesn't seem to be working..
 On macOS, inFrame transit effect won't work?
 On macOS El Capitan, the top bar won't hide. It's fine on High Sierra.
 On macOS, trayicon won't show up correctly. Black filled.->disabled
 On macOS, awaking from sleep >blank screen?
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LclType, LclProc, LclIntf, Menus, StdCtrls, ExtDlgs,
  strutils, Types, XMLConf{$ifdef windows}, Windows{$endif};

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Image1: TImage;
    MenuItem1: TMenuItem;
    MenuItemSysAbout: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemSysQuit: TMenuItem;
    MenuItemStretchIn: TMenuItem;
    MenuItemStretchOut: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemNext: TMenuItem;
    MenuItemBack: TMenuItem;
    MenuItemBo6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItemStayOnTop: TMenuItem;
    MenuItemSlideshow: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemSlideshowInFrame: TMenuItem;
    MenuItemQuit: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    PanelInframe: TPanel;
    PopupMenuSystem: TPopupMenu;
    PopupMenuMain: TPopupMenu;
    TimerEffectStart: TTimer;
    TrayIcon1: TTrayIcon;
    XMLConfig: TXMLConfig;
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Image1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Image1Resize(Sender: TObject);
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
    procedure PopupMenuMainPopup(Sender: TObject);
    procedure PopupMenuSystemPopup(Sender: TObject);
    procedure TimerEffectStartTimer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);

  private
    // Main file list.
    FstFileList:TStringList;
    FstDirectoryList:TStringList;
    FstPlaylistList:TStringList;
    FstMoniterList:TStringList;
    FstFileExtList:TStringList;
    FstPlaylistExtList:TStringList;

    // User Opts.
    FOptFullscreen:boolean;
    FOptTransitEffect:boolean;
    FOptFit:boolean;
    FOptExpand:boolean;
    FOptIntervalIntSeconds:integer;
    // Be carefull when setting this.
    FOptMinimulFileSizeKiloByte:integer;
    FOptRepeat:boolean;
    FOptRandom:boolean;
    FoptStayOnTop:boolean;
    FOptFileExts:string;
    FOptPlaylistExts:string;
    FOptIncludeSubFolders:boolean;
    FOptIntMoniter:integer;
    // Not using this for now.
    FoptFrameSkinWidth:integer;

    // App status.
    FisFullScreen: boolean;
    FisStartNormal: boolean;
    FOptStretch:boolean; //TODO: should remove "Opt" from name.
    FisInFrame:boolean;
    FOptSlideshowAutoStart:boolean; //TODO: should remove "Opt" from name.
    FisSingleFileSelected: boolean;
    FisManualTransition:boolean;
    FCurrentMonitor:TMonitor;
    FOrigBounds: TRect;
    FOrigWndState: TWindowState;
    FiCurrentFileIndex:integer;
    FstrAppVer:string;

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

  public
    property FileList: TStringList read FstFileList;
    property OptIntMoniter: integer read FOptIntMoniter write SetMoniter;
    property MoniterList: TStringList read FstMoniterList;
    property OptTransitEffect: boolean read FoptTransitEffect;
    property OptFit: boolean read FOptFit;
    property OptExpand: boolean read FOptExpand;
    property OptStretch: boolean read FOptStretch;
    property OptIntervalIntSeconds: integer read FOptIntervalIntSeconds;
    property OptMinimulFileSizeKiloByte: integer read FOptMinimulFileSizeKiloByte;
    property OptRandom: boolean read FOptRandom;
    property OptRepeat: boolean read FOptRepeat;
    property CurrentMonitor:TMonitor read GetCurrentMonitor;
    property IsInFrame:boolean read FisInFrame;
    property IsStartNormal:boolean read FisStartNormal;
    property OptStayOnTopInframe:boolean read FoptStayOnTop write SetStayOnTop;
    property OptSlideshowAutoStart:boolean read FOptSlideshowAutoStart;
    property IsSingleFileSelected: boolean read FisSingleFileSelected;
    property IsManualTransition: boolean read FisManualTransition;
    procedure DoneInFrame(strCurr:string);   // called from inFrame.
    procedure DoneFullscreen(strCurr:string);// called from fullscreen.
    procedure SetCaption(strCaption:string); // called from inFrame.

  end;

var
  frmMain: TfrmMain;

implementation

uses UFullscreen, UAbout;

{$R *.lfm}

{ TfrmMain }


procedure TfrmMain.FormCreate(Sender: TObject);
var
  s:string;
  i,j,f:integer;
  folderfiles:TStringlist;
  fileSearchMask,fileFolder:string;
begin
  FstrAppVer:='0.1.0';
  self.Caption:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');
  self.Visible:=false;

  self.AlphaBlend:=true;
  self.AlphaBlendValue:=255;
  TimerEffectStart.Enabled:=false;

  // Init main lists.
  FstFileList:=TStringList.create;
  FstDirectoryList:=TStringList.create;
  FstPlaylistList:=TstringList.Create;
  FstMoniterList:=TStringList.Create;

  // Set defaults for user options.
  FOptFullscreen:=false;
  FOptTransitEffect:=true;
  FOptExpand:=false;
  FOptFit:=true;
  FOptIntMoniter:=0;
  FOptIntervalIntSeconds:=4;
  FOptRandom:=true;
  FOptRepeat:=true;
  FOptFileExts:='.jpg;.jpeg;.jpe;.png;.gif';
  FOptPlaylistExts:='.m3u;.xspf';
  FOptIncludeSubFolders:=true;
  // Be carefull when settings this. If the size is too large, the list will be empty.
  FOptMinimulFileSizeKiloByte:=1;
  FoptFrameSkinWidth:=0;

  // Non user editable options. (TODO: should remove "Opt" from name.)
  FOptSlideshowAutoStart:=true;
  FOptStretch:=false;

  // Init other objects.
  FstFileExtList := TStringList.Create;
  FstFileExtList.Delimiter:=';';
  FstFileExtList.DelimitedText:=FOptFileExts;

  FstPlaylistExtList:= TStringList.Create;
  FstPlaylistExtList.Delimiter:=';';
  FstPlaylistExtList.DelimitedText:=FOptPlaylistExts;

  if Screen.MonitorCount > 1 then
  begin
    for i:=0 to Screen.MonitorCount-1 do
    begin
      FstMoniterList.Add(intToStr(i));
    end;
  end else
  begin
    FOptIntMoniter:=0;
  end;


  // Load settings
  if ForceDirectories(GetAppConfigDir(false)) then
  begin
    //XMLConfig.FileName:=GetAppConfigFile(False); // exename.cfg
    {$ifdef windows}
    XMLConfig.FileName:=GetAppConfigDir(false)+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'.config');
    {$else}
    XMLConfig.FileName:=GetAppConfigDir(false)+'.'+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'') +'.config';
    //TODO: Make it hidden file in Linux?
    {$endif}
  end else begin
    {$ifdef windows}
    XMLConfig.FileName:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'.config');
    {$else}
    XMLConfig.FileName:='.'+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'') +'.config';
    //TODO: Make it hidden file in Linux?
    {$endif}
  end;

  FOptRandom := XMLConfig.GetValue('/Opts/Random',FOptRandom);
  FOptRepeat := XMLConfig.GetValue('/Opts/Repeat',FOptRepeat);
  FOptFullscreen := XMLConfig.GetValue('/Opts/Fullscreen',FOptFullscreen);
  FOptTransitEffect := XMLConfig.GetValue('/Opts/TransitEffect',FOptTransitEffect);
  FOptFit := XMLConfig.GetValue('/Opts/Fit',FOptFit);
  FOptExpand := XMLConfig.GetValue('/Opts/Expand',FOptExpand);
  FOptIntMoniter := XMLConfig.GetValue('/Opts/Moniter',FOptIntMoniter);
  FOptMinimulFileSizeKiloByte := XMLConfig.GetValue('/Opts/MinimulFileSizeKiloByte',FOptMinimulFileSizeKiloByte);
  FoptStayOnTop := XMLConfig.GetValue('/Opts/StayOnTop',FoptStayOnTop);
  FOptIncludeSubFolders := XMLConfig.GetValue('/Opts/IncludeSubFolders',FOptIncludeSubFolders);
  FOptFileExts := XMLConfig.GetValue('/Opts/FileExts',FOptFileExts);
  FOptPlaylistExts := XMLConfig.GetValue('/Opts/PlaylistExts',FOptPlaylistExts);


  // Command line parameters

  // http://wiki.freepascal.org/Command_line_parameters_and_environment_variables
  // -h is a short option. The long form is the --help
  if Application.HasOption('h', 'help') then
  begin
    frmAbout := TfrmAbout.Create(self);
    frmAbout.Caption:=' '+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');
    frmAbout.StaticTextAppsVer.Caption := 'Image Viewer' + ' - ' + FstrAppVer;
    frmAbout.StaticTextWho.Caption := 'by torumyax';
    frmAbout.StaticTextWebSite.Caption:='https://github.com/torumyax/Image-viewer';
    frmAbout.ShowModal;
    Halt;
  end;

  // "-f filename" for a short form or "--file=filename" for a long form.
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
  if Application.HasOption('i', 'stretchIn') then
  begin
    if (LowerCase(Application.GetOptionValue('i', 'stretchIn')) = 'on') then
    begin
      FOptFit:=true;
    end else if (LowerCase(Application.GetOptionValue('i', 'stretchIn')) = 'off') then
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
      //TODO: Warn users.
      FOptMinimulFileSizeKiloByte:=i;
    end;
  end;
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
          if f >= (FOptMinimulFileSizeKiloByte * 1024) then
          begin
            FstFileList.Add(ParamStr(I));
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
    {$endif}
    end;

  end;

  // Search inside folder(s)
  if FstDirectoryList.Count > 0 then
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
    for i:=0 to FstDirectoryList.Count -1 do
    begin
      try
        // Recursively search files
        folderfiles := FindAllFiles(FstDirectoryList[i], fileSearchMask, FOptIncludeSubFolders);
        for j:=0 to folderfiles.Count - 1 do
        begin
          // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
          if not (AnsiStartsStr('.',ExtractFilename(folderfiles[j]))) then
          begin
            f:= FileSize(folderfiles[j]);
            if f >= (FOptMinimulFileSizeKiloByte * 1024) then
            begin
              FstFileList.Add(folderfiles[j]);
            end;
          end;
        end;
      finally
        folderfiles.Free;
      end;
    end;
  end;

  //TODO: playlist
  // Only if no other files are specified,
  // open the ploylist and add its contents to the filelist.


  if (FstFileList.Count < 1) then
  begin
    // No files are provided in the parameter string, so open "file open" dialog.
    OpenPictureDialog1.Title:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'')+' - Open picture file(s)';
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
            if f >= (FOptMinimulFileSizeKiloByte * 1024) then
            begin
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
  if FstFileList.Count = 1 then
  begin
    fileFolder:=ReplaceStr(FstFileList[0],ExtractFileName(FstFileList[0]),'');
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
          if (folderfiles[j] <> FstFileList[0]) then
          begin
            f:= FileSize(folderfiles[j]);
            if f >= (FOptMinimulFileSizeKiloByte * 1024) then
            begin
              FstFileList.Add(folderfiles[j]);
            end;
          end;
        end;
      end;
    finally
      folderfiles.Free;
    end;
    // Since automatically added, do not start slideshow at fullscreen.
    // FOptSlideshowAutoStart:=false;
    FisSingleFileSelected:=true;
  end;

  // We tried. But there is nothing to do, so self terminate.
  if (FstFileList.Count < 1) then
  begin
    Application.Terminate;
    exit;
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

    // Just in case set default;
    self.Top:=120;
    self.Left:=120;
    self.Width:=480;
    self.Height:=480;
    //
    self.AlphaBlend:=true;
    self.AlphaBlendValue:=255;
    self.Image1.Visible:=true;
    //
    FiCurrentFileIndex:=0;
    // Show.
    FisStartNormal := true;
    self.Show;
    //
    self.BringToFront;
    SetForegroundWindow(self.Handle);
  end;

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin

  if Application.Terminated then exit;

  if (FOptFullscreen and (not FisFullscreen) and (not FisStartNormal)) then
  begin
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
    // It must be in "FormShow"
    if fileexists(XMLConfig.FileName) then
    begin
       RestoreFormState;
    end;
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
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if FOptFullscreen or FisFullscreen then
  begin
    //TODO: Save fullscreen options
  end else begin
    // Save normal form size and pos.
    StoreFormState;
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

procedure TfrmMain.TimerEffectStartTimer(Sender: TObject);
begin

  //while doing this. user might click the main form ....
  //if done so, and popup won't work on fullscreen..
  //-> don't set popup menu in main form.

  if self.AlphaBlendValue < 255 then
  begin
    if (self.AlphaBlendValue < 100 ) then begin
       self.AlphaBlendValue := self.AlphaBlendValue + 10;
    end else
    begin
      if (self.AlphaBlendValue +5 >= 255) then begin
        self.AlphaBlendValue := 255;
      end else begin
        self.AlphaBlendValue := self.AlphaBlendValue + 5;
      end;
    end;
  end else begin
    TimerEffectStart.Enabled:=false;

    frmFullscreen := TfrmFullscreen.create(self);
    frmFullScreen.StartWith:=FiCurrentFileIndex;
    frmFullscreen.ShowModal;

    //returned from fullscreen
    if FisStartNormal then
    begin
      //TODO, if start with normal then, do not close.
      //TODO needs test.

      //TODO loadimage the last image fullscreen showed.

      Screen.Cursor:=crDefault;
      Screen.UpdateScreen;
      Image1.Visible:=true;
      ShowFullScreen(false);
      //we are back normal;
      FOptFullscreen:=false;
      //close;
    end else
    begin
      //start fullscreen so close after fullscreen.
      close; //when returned (frmFullscreen is closed), self close.
    end;

  end;
end;

procedure TfrmMain.TrayIcon1Click(Sender: TObject);
begin

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

procedure TfrmMain.LoadImage;
begin
  if FileList.Count > 0 then
  begin
    if (FileList.Count = 1) then
    begin
      Self.Caption:=FileList[FiCurrentFileIndex];
    end else
    begin
      Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
    end;
    Image1.Picture.LoadFromFile(FileList[FiCurrentFileIndex]);
    Image1Resize(nil);
  end;
end;

procedure TfrmMain.Image1Resize(Sender: TObject);
var
  curWidth,curHeight:integer;
begin
  if not Assigned(Image1.Picture) then exit;

  Image1.Stretch:=false;
  //Image1.StretchInEnabled:=false;
  //Image1.StretchOutEnabled:=false;

  curWidth := self.ClientWidth; //.Width;
  curHeight:= self.ClientHeight; //.Height;
  //if FOptStretch then begin
  //   Image1.Stretch:=true;
  //end else begin
    if FOptFit then
    begin
      //fit only when larger than screen size.

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
  //end;
  //Image1.Refresh;
  //Image1.Repaint;
end;

procedure TfrmMain.Image1DblClick(Sender: TObject);
begin
  if FisInFrame then
  begin
    // This shuldn't be happening.
  end else
  begin
    if not FisFullscreen then
    begin
      MenuItemSlideshowClick(nil);
    end else
    begin
      // This shuldn't be happening.
    end;
  end;
end;

procedure TfrmMain.MenuItemSlideshowClick(Sender: TObject);
begin
  // Start fullscreen
  if (not FisFullscreen) and (not FisInFrame) then
  begin
    //TODO: do not change option value...
    FOptFullscreen:=true;

    // FisStartNormal, so this is telling TimerEffectStart to go back to normal when done.
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
      frmFullscreen.ShowModal;

      // DoneFullscreen will be called

      Image1.Visible:=true;
      ShowFullScreen(false);
      Screen.Cursor:=crDefault;
    end;

  end;
end;

procedure TfrmMain.Image1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (not FisFullscreen) and (not FisInFrame) then
  begin
    if FiCurrentFileIndex < FileList.Count -1 then
    begin
     screen.Cursor:=crHourGlass;
     Image1.Picture.LoadFromFile(FileList[FiCurrentFileIndex+1]);
     Image1Resize(self);
     FiCurrentFileIndex:=FiCurrentFileIndex+1;
     Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
     screen.Cursor:=crDefault;
     //Handled:=true;
    end;
  end;
end;

procedure TfrmMain.Image1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (not FisFullscreen) and (not FisInFrame) then
  begin
    if FiCurrentFileIndex > 0 then
    begin
     screen.Cursor:=crHourGlass;
     Image1.Picture.LoadFromFile(FileList[FiCurrentFileIndex-1]);
     Image1Resize(self);
     FiCurrentFileIndex:=FiCurrentFileIndex-1;
     Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
     screen.Cursor:=crDefault;
     //Handled:=true;
    end;
  end;
end;

procedure TfrmMain.MenuItemNextClick(Sender: TObject);
begin
  if FiCurrentFileIndex < FileList.Count -1 then
  begin
   screen.Cursor:=crHourGlass;
   Image1.Picture.LoadFromFile(FileList[FiCurrentFileIndex+1]);
   Image1Resize(self);
   FiCurrentFileIndex:=FiCurrentFileIndex+1;
   Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
   screen.Cursor:=crDefault;
  end;

end;

procedure TfrmMain.MenuItemBackClick(Sender: TObject);
begin
  if FiCurrentFileIndex > 0 then
  begin
   screen.Cursor:=crHourGlass;
   Image1.Picture.LoadFromFile(FileList[FiCurrentFileIndex-1]);
   Image1Resize(self);
   FiCurrentFileIndex:=FiCurrentFileIndex-1;
   Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
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
end;

procedure TfrmMain.MenuItemSlideshowInFrameClick(Sender: TObject);
{$ifdef windows}{$else}var
  Form: TForm;{$endif}
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
        //todo do I need to do something?? need to check.
      end;
    end;
  end else
  begin
    {$ifdef windows}
    FOrigBounds:= BoundsRect;
    self.BorderStyle:=bsNone;
    BoundsRect := FOrigBounds;
    {$else}
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

    FisInFrame:=true;

    self.Caption:='InFrame Slideshow';

    Image1.Visible:=false;

    frmFullscreen := TfrmFullscreen.create(self);
    frmFullScreen.StartWith:=FiCurrentFileIndex;

    //frmFullscreen.Parent := self;
    //frame skin test
    PanelInframe.Top:=FoptFrameSkinWidth;
    PanelInframe.left:=FoptFrameSkinWidth;
    PanelInframe.Height:=self.ClientHeight-(PanelInframe.Top*2);
    PanelInframe.Width:=self.ClientWidth-(PanelInframe.left*2);
    PanelInframe.Visible:=true;
    PanelInframe.BringToFront;
    frmFullscreen.Parent := PanelInframe;

    frmFullscreen.Visible:=true;
    frmFullscreen.Show;

    //inFrame you have to
    //call frmFullscreen.FormResize(self);   when resize
    //call DoneInFrame(); from frmFullscreen when close fullscreen.
  end;
end;

procedure TfrmMain.DoneInFrame(strCurr :string);
var{$ifdef windows}{$else}
  Form: TForm;{$endif}
  i:integer;
begin
  FisInFrame:=false;

  {$ifdef windows}
  BoundsRect:= FOrigBounds;
  self.BorderStyle:=bsSizeable;
  BoundsRect:=FOrigBounds;
  {$else}
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

  PanelInframe.Visible:=false;

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
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if FisInFrame then
  begin
    if assigned(frmFullscreen) and frmFullscreen.Visible then
    begin
      frmFullscreen.FormResize(self);
    end;
  end;
end;

procedure TfrmMain.SetCaption(strCaption:string);
begin
  Self.Caption:= strCaption;
end;

procedure TfrmMain.SetStayOnTop(bln:Boolean);
begin
  if bln then
  begin
    self.FormStyle:=fsSystemStayOnTop;
    MenuItemStayOnTop.Checked:=true;
  end else
  begin
    self.FormStyle:=fsNormal;
    MenuItemStayOnTop.Checked:=false;
  end;
  self.FoptStayOnTop:=bln;
end;

procedure TfrmMain.ApplicationProperties1Exception(Sender: TObject; E: Exception);
begin
  //TODO: oops
  // We don't want to show dialog while showing fullscreen with Modal window.
  // e.g. message dialog showing behind the modal window and no way to click or see.
  // so let's just terminate.
  // TODO: error logging.

  //Application.Terminate;
  //Halt(1);

  // http://lists.elists.org/pipermail/delphi/2000-September/010939.html
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
        //TODO:
        {
        if ((Key = VK_RMENU) or (Key = VK_LMENU)) then
        }
      end;

    end;
  end else if FOptFullscreen and FisFullscreen then
  begin
    if ((Key = VK_F11) or (Key = VK_ESCAPE)) then
    begin
      close;
    end;
  end else begin
    // Normal image view.

    if (Chr(Key) = 'Q') then
    begin
      if (ssCtrl in Shift) then
      begin
        close;
      end;
    end;

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
  if (not FisFullscreen) and (not FisInFrame) then
  begin
    frmAbout := TfrmAbout.Create(self);
    frmAbout.Caption:=' '+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');

    frmAbout.StaticTextAppsVer.Caption := 'Image Viewer' + ' - ' + FstrAppVer;
    frmAbout.StaticTextWho.Caption := 'by torumyax';
    frmAbout.StaticTextWebSite.Caption:='https://github.com/torumyax/Image-viewer';

    frmAbout.ShowModal;
    // no need to free. closeAction caFree.
  end;
end;

procedure TfrmMain.MenuItemSysQuitClick(Sender: TObject);
begin
  close;
end;

procedure TfrmMain.PopupMenuMainPopup(Sender: TObject);
begin
  if (FstFileList.Count > 1) then
  begin
    //MenuItemSlideshowInFrame.Visible:=true;

    //if FisManualTransition then
    if FisSingleFileSelected then
    begin
      MenuItemSlideshowInFrame.Caption:='View in &InFrame';
      MenuItemSlideshow.Caption:='View in &Fullscreen';
    end else
    begin
      MenuItemSlideshowInFrame.Caption:='Start &InFrame Slideshow';
      MenuItemSlideshow.Caption:='Start &Fullscreen Slideshow';
    end;
  end else
  begin
    //MenuItemSlideshowInFrame.Visible:=false;
    MenuItemSlideshowInFrame.Caption:='&InFrame View';
    MenuItemSlideshow.Caption:='&Fullscreen View';

    MenuItemNext.enabled:=false; //visible or enabled:=false;
    MenuItemBack.enabled:=false;
  end;

  if FOptFit then MenuItemStretchIn.Checked:=true else MenuItemStretchIn.Checked:=false;
  if FOptExpand then MenuItemStretchOut.Checked:=true else MenuItemStretchOut.Checked:=false;

end;

procedure TfrmMain.PopupMenuSystemPopup(Sender: TObject);
begin
  if (FisFullscreen) and (FisInFrame) then
  begin
    MenuItemSysAbout.Enabled:=false;
  end else
  begin
    MenuItemSysAbout.Enabled:=true;
  end;
end;



procedure TfrmMain.SetMoniter(intMoniter:integer);
begin
  //TODO validate input value
  FOptIntMoniter:=intMoniter;

  if FOptFullscreen and FisFullScreen and Assigned(frmFullscreen) then
  begin
      ShowFullScreen(true);
  end else
  begin
    // non fullscreen.
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

  // Save options.
  XMLConfig.SetValue('/Opts/Random',FOptRandom);
  XMLConfig.SetValue('/Opts/Repeat',FOptRepeat);
  XMLConfig.SetValue('/Opts/Fullscreen',FOptFullscreen);
  XMLConfig.SetValue('/Opts/TransitEffect',FOptTransitEffect);
  XMLConfig.SetValue('/Opts/Fit',FOptFit);
  XMLConfig.SetValue('/Opts/Expand',FOptExpand);
  XMLConfig.SetValue('/Opts/Moniter',FOptIntMoniter);
  XMLConfig.SetValue('/Opts/MinimulFileSizeKiloByte',FOptMinimulFileSizeKiloByte);
  XMLConfig.SetValue('/Opts/StayOnTop',FoptStayOnTop);
  XMLConfig.SetValue('/Opts/IncludeSubFolders',FOptIncludeSubFolders);
  XMLConfig.SetValue('/Opts/FileExts',FOptFileExts);
  XMLConfig.SetValue('/Opts/PlaylistExts',FOptPlaylistExts);

end;

end.

























