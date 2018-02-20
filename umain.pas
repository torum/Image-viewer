unit UMain;

{$mode objfpc}{$H+}

{
Memo:
 Right now, the code is a mess. Once I finish implementing all the
 basic functionality, I have to clean this up.
}

{
Source:
 https://github.com/torumyax/Image-viewer

No extra components required.

Tested on
 Windows 10: Lazarus 1.8.0 r56594 FPC 3.0.4 x86_64-win64-win32/win64
 Ubuntu 17.10: Lazarus 1.8.0 rc4+dfsg-1 FPC 3.0.2 x86_64-linux-gtk2
 macOS 10.03.2 High Sierra on iMac(21.5 Inch, Late 2012 - Intel Core i5, 8GB memory).
               Lazarus 1.8.0 rexported FPC 3.0.4 i386-darwin-carbon
 Ubuntu 16.04 LTS

TODO:
 Save and load options.
 Command line options.
 GUI settings.
 stop using config xml use ini.
 file drop handling.
 PreLoading image for slideshow.
 load playlist.
 OS-power-save event aware.
 i18n
  http://wiki.lazarus.freepascal.org/Step-by-step_instructions_for_creating_multi-language_applications
  http://wiki.lazarus.freepascal.org/Translations_/_i18n_/_localizations_for_programs
 UWP packaging and release.


Known issue and bugs:
 On Windows, PNG (depth 24) antialising isn't working when stretch.
  https://forum.lazarus.freepascal.org/index.php?topic=24408.0
  http://forum.lazarus.freepascal.org/index.php?topic=19542.0

 On Ubuntu(and MacOS), bsNone won't work. titlebar/border won't hide.
 Fixed->On Ubuntu 16.04 with unity won't show fullscreen.
 Fixed->On Ubuntu, modal window won't become fullscreen. "Top bar" and "Dock" won't hide.
  https://forum.lazarus.freepascal.org/index.php/topic,40151.0.html
 On Ubuntu, OpenPictureDialog won't show thumbnails?

 On macOS, fullscreen won't hide top bar and dock. But my sample works fine....
 On macOS, bsNone won't work. titlebar/border won't hide.
 On macOS, inFrame transit effect won't work?
 On macOS, trayicon won't show correctly. Black filled.->disabled
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
    MenuItemStretchIn: TMenuItem;
    MenuItemStretchOut: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemNext: TMenuItem;
    MenuItemBack: TMenuItem;
    MenuItem6: TMenuItem;
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
    procedure MenuItemSlideshowClick(Sender: TObject);
    procedure MenuItemSlideshowInFrameClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemStayOnTopClick(Sender: TObject);
    procedure MenuItemStretchInClick(Sender: TObject);
    procedure MenuItemStretchOutClick(Sender: TObject);
    procedure PopupMenuMainPopup(Sender: TObject);
    procedure TimerEffectStartTimer(Sender: TObject);

  private
    FstFileList:TStringList;
    FstDirectoryList:TStringList;
    FstPlaylistList:TStringList;
    FstMoniterList:TStringList;
    FstFileExtList:TStringList;
    FstPlaylistExtList:TStringList;
    //opts
    FOptFullscreen:boolean;
    FOptTransitEffect:boolean;
    FOptStretch:boolean;
    FOptFit:boolean;
    FOptExpand:boolean;
    FOptIntervalIntSeconds:integer;
    FOptMinimulFileSizeKiloByte:integer; //be carefull when setting this

    FOptRepeat:boolean;
    FOptRandom:boolean;
    FoptStayOnTopInframe:boolean;

    FOptFileExts:string;
    FOptPlaylistExts:string;

    FOptIncludeSubFolders:boolean;
    FOptSlideshowAutoStart:boolean;

    FoptFrameSkinWidth:integer; //not using for now

    //app status
    FisFullScreen: boolean;
    FisStartNormal: boolean;
    FisSingleFileSelected: boolean;
    FisManualTransition:boolean;
    FOptIntMoniter:integer;
    FisInFrame:boolean;
    FCurrentMonitor:TMonitor;

    FOrigBounds: TRect;
    FOrigWndState: TWindowState;

    FiCurrentFileIndex:integer;

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
    //property IsCustumScreen: boolean read FisCustumScreen;
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
    property OptStayOnTopInframe:boolean read FoptStayOnTopInframe write SetStayOnTop;
    property OptSlideshowAutoStart:boolean read FOptSlideshowAutoStart;
    property IsSingleFileSelected: boolean read FisSingleFileSelected;
    property IsManualTransition: boolean read FisManualTransition;
    procedure DoneInFrame(strCurr:string);
    procedure DoneFullscreen(strCurr:string);
    procedure SetCaption(strCaption:string);

  end;

var
  frmMain: TfrmMain;

implementation

uses UFullscreen;

{$R *.lfm}

{ TfrmMain }


procedure TfrmMain.FormCreate(Sender: TObject);
var
  i,j,f:integer;
  folderfiles:TStringlist;
  fileSearchMask,fileFolder:string;
begin

  self.Caption:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');
  self.Visible:=false;

  self.AlphaBlend:=true;
  self.AlphaBlendValue:=255;
  TimerEffectStart.Enabled:=false;

  //init
  FstFileList:=TStringList.create;
  FstDirectoryList:=TStringList.create;
  FstPlaylistList:=TstringList.Create;
  FstMoniterList:=TStringList.Create;

  //set defaults
  FOptFullscreen:=false;
  FOptTransitEffect:=true;
  FOptStretch:=false;
  FOptExpand:=false;
  FOptFit:=true;
  FOptIntMoniter:=0;
  FOptIntervalIntSeconds:=4;
  FOptMinimulFileSizeKiloByte:=1;//don't change

  FOptRandom:=true;
  FOptRepeat:=true;

  FOptFileExts:='.jpg;.jpeg;*.jpe;.png;.gif';
  FOptPlaylistExts:='.m3u;.xspf';

  FOptIncludeSubFolders:=true;
  FOptSlideshowAutoStart:=true;

  FoptFrameSkinWidth:=0;

  FstFileExtList := TStringList.Create;
  FstFileExtList.Delimiter:=';';
  FstFileExtList.DelimitedText:=FOptFileExts;

  FstPlaylistExtList:= TStringList.Create;
  FstPlaylistExtList.Delimiter:=';';
  FstPlaylistExtList.DelimitedText:=FOptPlaylistExts;


  //parse prameter string.
  for I := 1 to ParamCount do
  begin
    if (AnsiStartsStr('-',ParamStr(I))) then
    begin
      //options

    end else if (FileExists(ParamStr(I))) then
    begin
      //found a file
      {$ifdef windows}
      {$else}
      //on unix, directory is also file.
      if (DirectoryExists(ParamStr(I))) then begin
        //found a folder
        if not (AnsiStartsStr('.',ExtractFilename(ParamStr(I)))) then
        begin
          FstDirectoryList.Add(ParamStr(I));
        end;
        Continue;
      end;
      {$endif}

      if (FstFileExtList.IndexOf(LowerCase(ExtractFileExt(ParamStr(I)))) >= 0) then
      begin
        //is picture file
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
        //found a playlist
        FstPlaylistList.Add(ParamStr(I));
      end;

    {$ifdef windows}
    end else if (DirectoryExists(ParamStr(I))) then
    begin
      //found a folder
      if not (AnsiStartsStr('.',ExtractFilename(ParamStr(I)))) then
      begin
        FstDirectoryList.Add(ParamStr(I));
      end;
    {$endif}
    end;

  end;

  //search inside folder(s)
  if FstDirectoryList.Count > 0 then
  begin
    //create search mask
    fileSearchMask:='';
    for i:=0 to FstFileExtList.Count-1 do
    begin
      if trim(FstFileExtList[i]) <> '' then begin
         fileSearchMask:= fileSearchMask+'*'+trim(FstFileExtList[i])+';';
      end;
    end;
    //loop
    for i:=0 to FstDirectoryList.Count -1 do
    begin
      try
        //Application.ProcessMessages;
        //recursively search files
        folderfiles := FindAllFiles(FstDirectoryList[i], fileSearchMask, FOptIncludeSubFolders); //'*.jpg;*.jpeg;*.png;'
        for j:=0 to folderfiles.Count - 1 do
        begin
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

  //TODO playlist
  //open ploylist files and add them to the filelist.
  //when only if no other files are specified.
  //FstPlaylistList.Count
  //.m3u;.xspf



  if Application.HasOption('h', 'help') then
  begin
    //todo
  end;

  if (FstFileList.Count < 1) then begin
    //No files are provided in the parameter string, so open "file open" dialog.
    //TODO modal dialog with options and playlist edit tab (drag & drop files) and "About" tab.

    OpenPictureDialog1.Title:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'')+' - Open picture file(s)';
    if OpenPictureDialog1.Execute then
    begin
      for i:=0 to OpenPictureDialog1.Files.Count -1 do
      begin
        if (FstFileExtList.IndexOf(LowerCase(ExtractFileExt(OpenPictureDialog1.Files[i]))) >= 0) then
        begin
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
          //playlist
          FstPlaylistList.Add(OpenPictureDialog1.Files[i]);
        end;
      end;
    end;

    //TODO playlist
    //open ploylist files and add them to the filelist.

  end;

  //if only one image was selected, add all siblings automatically.
  //since "send to" command-line parameters don't accept more than 255.
  if FstFileList.Count = 1 then
  begin
    fileFolder:=ReplaceStr(FstFileList[0],ExtractFileName(FstFileList[0]),'');
    //create search mask
    fileSearchMask:='';
    for i:=0 to FstFileExtList.Count-1 do
    begin
      if trim(FstFileExtList[i]) <> '' then begin
         fileSearchMask:= fileSearchMask+'*'+trim(FstFileExtList[i])+';';
      end;
    end;
    try
      //find siblings.
      folderfiles := FindAllFiles(fileFolder, fileSearchMask, false);
      for j:=0 to folderfiles.Count - 1 do
      begin
        if not (AnsiStartsStr('.',ExtractFilename(folderfiles[j]))) then
        begin
          //ignore first selected image.
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
    //Since automatically added, do not start slideshow at fullscreen.
    //FOptSlideshowAutoStart:=false;
    FisSingleFileSelected:=true;
  end;



  //We tried. But there is nothing to do, so self terminate.
  if (FstFileList.Count < 1) then begin
    Application.Terminate;
    exit;
  end;

  if Screen.MonitorCount > 1 then
  begin
    for i:=0 to Screen.MonitorCount-1 do
    begin
      FstMoniterList.Add(intToStr(i));
    end;
  end else
  begin
    FOptIntMoniter:=0; //let's make sure.
  end;



  //TODO opts-> (i)interval(3) (e)repeat (r)random (f)fullscreen (t)effect (m)moniter(1)
  //(p)windowPosition(24,24) (s)windowSize(240x380) (o)stayontop
  //(x)fileExt(.jpg;.jpeg;.png) (t)playlistExt(.imagelist) (u)minimuFileSizeKB(1)
  //(c)stretch (a)includSubfolders (h)help
  if Application.HasOption('i', 'interval') then begin
    //WriteLn('');

    // http://wiki.freepascal.org/Command_line_parameters_and_environment_variables
    // -f filename or --file=filename
    // GetOptionValue('f', 'file')
  end;
  //start-at filename-sorting show-filename show-on-multiple-displays random-background
  //show-fullpath show-filename



  {$ifdef windows}
  //System TrayIcon
  TrayIcon1.Visible:=true;
  TrayIcon1.Hint:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');
  //TODO + moniter[1]    etc
  {$else}
  TrayIcon1.Visible:=false;
  {$endif}


  //load ini settings
  // since linux don't have ext(.exe or any ext), so... replace and append ext.
  XMLConfig.FileName:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'') +'.xml';


  if FOptFullscreen then
  begin
    //start fullscreen.

    //main form become just background.
    self.ShowInTaskBar:=stNever;
    self.Image1.Visible:=false;

    //self.WindowState:=wsFullScreen;
    self.AlphaBlend:=true;
    self.AlphaBlendValue:=1;

    FisStartNormal:=false;
    self.Show;
    self.BringToFront;

  end else
  begin

    //main form is the viewer.
    self.ShowInTaskBar:=stDefault;

    self.Position:=poDefault;
    self.WindowState:=wsNormal;

    //just in case set default;
    self.Top:=120;
    self.Left:=120;
    self.Width:=480;
    self.Height:=480;

    //this won't work here. do it at FormShow.
    //if fileexists(XMLConfig.FileName) then begin
    //   RestoreFormState;
    //end;
    self.AlphaBlend:=true;
    self.AlphaBlendValue:=255;
    self.Image1.Visible:=true;

    FiCurrentFileIndex:=0;

    FisStartNormal := true;
    self.Show;

    self.BringToFront;
    SetForegroundWindow(self.Handle);
  end;

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin

  if Application.Terminated then exit;

  //if FOptFullscreen then
  if (FOptFullscreen and (not FisFullscreen) and (not FisStartNormal)) then
  begin
    //Screen.Cursor:=crNone;
    //Screen.UpdateScreen;

    if FOptTransitEffect then
    begin
      self.AlphaBlendValue:=1;
      ShowFullScreen(true);
      //start transition timer, and timer creates fullscreen
      TimerEffectStart.Enabled:=true;
    end else
    begin
      //Create fullscreen form at "FormActivate" >why?
      self.AlphaBlendValue := 1;
      ShowFullScreen(true);
      self.AlphaBlendValue := 255;
      self.BringToFront;
      //SetForegroundWindow(self.Handle);
    end;
  end else if FIsInFrame then begin
    //?
  end else
  begin
    //it seems, it must be in "FormShow"
    if fileexists(XMLConfig.FileName) then begin
       RestoreFormState;
    end;

    LoadImage;

  end;

  //just in case
  if FisFullscreen then
  begin
    if Assigned(frmFullscreen) then
    begin
      frmFullscreen.BringToFront;
      SetForegroundWindow(frmFullscreen.Handle);
    end else begin
      //something went wrong....
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

      close; //when returned (frmFullscreen is closed), self close.
    end;
  end;


  //just in case
  if FisFullscreen then
  begin
    if Assigned(frmFullscreen) then
    begin
      frmFullscreen.BringToFront;
      SetForegroundWindow(frmFullscreen.Handle);
    end else begin
      //something went wrong....
      FisFullscreen:=false;
    end;
  end;

end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if FOptFullscreen or FisFullscreen then
  begin
    //todo fullscreen options
  end else begin
    //saves normal form size and pos.
    StoreFormState;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
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

procedure TfrmMain.LoadImage;
begin
  if FileList.Count > 0 then begin
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

  //TODO: don't
  Image1.Stretch:=false;
  //Image1.StretchInEnabled:=false;
  //Image1.StretchOutEnabled:=false;

  curWidth := self.ClientWidth; //.Width;
  curHeight:= self.ClientHeight; //.Height;
  //if FOptStretch then begin
  //   Image1.Stretch:=true;
  //end else begin
    if FOptFit then begin
      //fit only when larger than screen size.

      if ((Image1.Picture.Width > curWidth) or
              (Image1.Picture.height > curHeight)) then begin
        Image1.Stretch:=true;
        Image1.StretchInEnabled:=true;
        Image1.StretchOutEnabled:=true;
        Image1.AntialiasingMode:=amOn;

      end else begin
        Image1.Stretch:=false;
        Image1.StretchInEnabled:=false;
        Image1.StretchOutEnabled:=false;
        Image1.AntialiasingMode:=amOff;
      end;
    end else begin
      Image1.Stretch:=false;
      Image1.StretchInEnabled:=false;
      Image1.StretchOutEnabled:=false;
      Image1.AntialiasingMode:=amOff;
    end;

    if FOptExpand then begin
      if ((Image1.Picture.Width < curWidth) and
              (Image1.Picture.height < curHeight)) then begin
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
  if FisInFrame then begin
    //this shuldn't be happening.
  end else
  begin
    if not FisFullscreen then
    begin
      MenuItemSlideshowClick(nil);
    end else
    begin
      //this shuldn't be happening.
    end;
  end;
end;

procedure TfrmMain.MenuItemSlideshowClick(Sender: TObject);
begin

  //start fullscreen
  if (not FisFullscreen) and (not FisInFrame) then
  begin
    //TODO, do not change option value...
    FOptFullscreen:=true;

    FisStartNormal:=true;//so tell TimerEffectStart to go back to normal when done.

    Image1.Visible:=false;

    //sets current screen of the form.
    FCurrentMonitor:=Screen.MonitorFromWindow(handle);
    FOptIntMoniter:=getCurrentMonitorIndex();

    if FOptTransitEffect then
    begin
      self.AlphaBlendValue:=1; //TODO form dissapears at once. maybe fadeout timer?
      ShowFullScreen(true);

      //start transition timer, and timer creates fullscreen
      TimerEffectStart.Enabled:=true;
    end else
    begin

      ShowFullScreen(true);

      frmFullscreen := TfrmFullscreen.create(self);
      frmFullScreen.StartWith:=FiCurrentFileIndex;
      frmFullscreen.ShowModal;

      //DoneFullscreen will be called

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
    if FiCurrentFileIndex < FileList.Count -1 then begin
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
    if FiCurrentFileIndex > 0 then begin
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
  {
  if FiCurrentFileIndex < FileList.Count -1 then begin
   screen.Cursor:=crHourGlass;
   Image1.Picture.LoadFromFile(FileList[FiCurrentFileIndex+1]);
   FiCurrentFileIndex:=FiCurrentFileIndex+1;
   Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
   screen.Cursor:=crDefault;
  end;
  }
end;

procedure TfrmMain.MenuItemBackClick(Sender: TObject);
begin
  {
  if FiCurrentFileIndex > 0 then begin
   screen.Cursor:=crHourGlass;
   Image1.Picture.LoadFromFile(FileList[FiCurrentFileIndex-1]);
   FiCurrentFileIndex:=FiCurrentFileIndex-1;
   Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
   screen.Cursor:=crDefault;
  end;
  }
end;


procedure TfrmMain.DoneFullscreen(strCurr:string);
var
  i:integer;
begin
  if FIsStartNormal then
  begin
    if FileList.Count <= 1 then exit;  //must be here. one pic fullscreen hangs

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
      //TODO. don't know how to hide titlebar in linux.
    {$endif}

    FisInFrame:=true;

    self.Caption:='InFrame Slideshow';

    Image1.Visible:=false;

    frmFullscreen := TfrmFullscreen.create(self);
    //frmFullScreen.WindowState:=wsNormal;
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
var
  i:integer;
begin

  FisInFrame:=false;

  {$ifdef windows}
  BoundsRect:= FOrigBounds;
  self.BorderStyle:=bsSizeable;
  BoundsRect:=FOrigBounds;
  {$else}
    //TODO. don't work on Ubuntu.
  {$endif}


  //self.BoundsRect := frmFullscreen.BoundsRect;

  //test end
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

  {
  FiCurrentFileIndex:=iCurr;
  LoadImage();
  }

  Image1.Repaint;
  Image1.Refresh;
  Image1.BringToFront;

  screen.Cursor:=crDefault;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if FisInFrame then begin
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
  //if (self.FormStyle = fsSystemStayOnTop) then
  if bln then
  begin
    self.FormStyle:=fsSystemStayOnTop;
    MenuItemStayOnTop.Checked:=true;
  end else
  begin
    self.FormStyle:=fsNormal;
    MenuItemStayOnTop.Checked:=false;
  end;
  self.FoptStayOnTopInframe:=bln;
end;

procedure TfrmMain.ApplicationProperties1Exception(Sender: TObject; E: Exception);
begin
  //TODO oops
  //We don't want to show dialog while showing fullscreen with Modal window.
  //e.g. message dialog showing behind the modal window and no way to click or see.
  //so let's just terminate.
  //TODO logging.

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

        //pass it to fullscreen.
        if ((Key = VK_F11) or (Key = VK_ESCAPE) or (Chr(Key) = 'F') or (Chr(Key) = 'S')) then
        begin
          //close;
          frmFullscreen.Close;
        end;
        if (Key = VK_RIGHT) then begin
          frmFullscreen.PlaybackNext(sender);
        end;
        //back
        if (Key = VK_LEFT) or (Key = VK_BACK) then begin
          frmFullscreen.PlaybackBack(sender);
        end;
        //pause/start
        if (Key = VK_PAUSE) or (Key = VK_SPACE) then
        begin
          frmFullscreen.PlaybackPause(sender);
        end;
        //TODO inFrame: frmMain steals fullscreen's shortcut keys. so.
        if (Chr(Key) = 'I') then
        begin
          //stretch in
          frmFullscreen.MenuItemFitClick(sender);
        end;
        if (Chr(Key) = 'O') then
        begin
          //stretch out
          frmFullscreen.MenuItemExpandClick(sender);
        end;
        if (Chr(Key) = 'E') then
        begin
          //effect
          frmFullscreen.MenuItemEffectClick(sender);
        end;
        if (Chr(Key) = 'N') then
        begin
          //random
          frmFullscreen.MenuItemRandomClick(sender);
        end;
        if (Chr(Key) = 'R') then
        begin
          //repeat
          frmFullscreen.MenuItemRepeatClick(sender);
        end;
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
    // normal image view.

    if (Chr(Key) = 'Q') then begin
      close;
    end;

    //Since form steals Image menu...

    //fullscreen slideshow
    if ((Key = VK_F11) or (Chr(Key) = 'F')) then
    begin
     MenuItemSlideshowClick(nil);
    end;


    //InFrame slideshow
    if (Chr(Key) = 'S') then
    begin
     MenuItemSlideshowInFrameClick(nil);
    end;


    //next
    if (Key = VK_RIGHT) then begin
      if FiCurrentFileIndex < FileList.Count -1 then begin
       screen.Cursor:=crHourGlass;
       Image1.Picture.LoadFromFile(FileList[FiCurrentFileIndex+1]);
       Image1Resize(self);
       FiCurrentFileIndex:=FiCurrentFileIndex+1;
       Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
       screen.Cursor:=crDefault;
      end;
    end;
    //back
    if (Key = VK_LEFT) or (Key = VK_BACK) then begin
      if FiCurrentFileIndex > 0 then begin
       screen.Cursor:=crHourGlass;
       Image1.Picture.LoadFromFile(FileList[FiCurrentFileIndex-1]);
       Image1Resize(self);
       FiCurrentFileIndex:=FiCurrentFileIndex-1;
       Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + FileList[FiCurrentFileIndex];
       screen.Cursor:=crDefault;
      end;
    end;
    //stretch in
    if (Chr(Key) = 'I') then begin
      MenuItemStretchInClick(nil);
    end;
    //stretch out
    if (Chr(Key) = 'O') then begin
      MenuItemStretchOutClick(nil);
    end;

  end;
end;

procedure TfrmMain.MenuItemQuitClick(Sender: TObject);
begin
  close;
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



procedure TfrmMain.SetMoniter(intMoniter:integer);
begin
  //TODO validate input value
  FOptIntMoniter:=intMoniter;

  if FOptFullscreen and FisFullScreen and Assigned(frmFullscreen) then
  begin
      ShowFullScreen(true);
  end else
  begin
    //non fullscreen.
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
begin
  if blnOn then
  begin
    if not FisFullScreen then
    begin
      //save original windowstate
      FOrigWndState:= WindowState;
      FOrigBounds := BoundsRect;
    end;
    //WindowState:=wsFullScreen; //don't do this if the form is modal on linux.
    {$ifdef windows}
    BorderStyle:= bsNone;  //don't do this at runtime on linux!
    {$endif}
    if (CurrentMonitor <> Screen.Monitors[FOptIntMoniter]) then
    begin
      BoundsRect:= Screen.Monitors[FOptIntMoniter].BoundsRect;
    end else
    begin
      BoundsRect:= CurrentMonitor.BoundsRect;
    end;
    //ShowWindow(Handle, SW_SHOWFULLSCREEN);
  end else
  begin
    WindowState:= FOrigWndState;
    {$ifdef windows}
    BorderStyle:= bsSizeable;  //don't do this at runtime on linux!
    {$endif}
    //ShowWindow(Handle, SW_SHOWNORMAL);
    BoundsRect:= FOrigBounds;
  end;
end;

procedure TfrmMain.SetFullScreen_Win32(blnOn: boolean);
begin
  if blnOn then
  begin
    //self.AlphaBlendValue:=1;

    if not FisFullScreen then
    begin
      FOrigWndState:= WindowState;
      FOrigBounds:= BoundsRect;
      //must be this order
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

    //ShowWindow(Handle, SW_SHOWFULLSCREEN);
  end else
  begin
    WindowState:= FOrigWndState;
    BoundsRect:= FOrigBounds;
    BorderStyle:= bsSizeable;
    //ShowWindow(Handle, SW_SHOWNORMAL);
    BoundsRect:= FOrigBounds; //again
  end;
end;



// http://wiki.freepascal.org/Remember_form_position_and_size

procedure TfrmMain.RestoreFormState;
var
  LastWindowState: TWindowState;
begin

  with XMLConfig do begin
    LastWindowState := TWindowState(GetValue('WindowState', Integer(WindowState)));

    if LastWindowState = wsMaximized then begin
      WindowState := wsNormal;
      BoundsRect := Bounds(
        GetValue('RestoredLeft', RestoredLeft),
        GetValue('RestoredTop', RestoredTop),
        GetValue('RestoredWidth', RestoredWidth),
        GetValue('RestoredHeight', RestoredHeight));
      WindowState := wsMaximized;
    end else begin
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

  with XMLConfig do begin
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


end.

