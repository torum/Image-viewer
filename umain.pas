unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LclType, LclProc, LclIntf, Menus, StdCtrls, ExtDlgs,
  XMLPropStorage, strutils, Types, XMLConf;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Image1: TImage;
    MenuItemQuit: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    PopupMenu1: TPopupMenu;
    TimerEffectStart: TTimer;
    TrayIcon1: TTrayIcon;
    XMLConfig: TXMLConfig;
    XMLPropStorage: TXMLPropStorage;
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure ApplicationProperties1Restore(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Image1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure TimerEffectStartTimer(Sender: TObject);

  private
    FstFileList:TStringList;
    FstDirectoryList:TStringList;
    FstPlaylistList:TStringList;
    FstMoniterList:TStringList;

    FOptIntMoniter:integer;//todo multiple moniters

    FOptFileExts:string;
    FstFileExtList:TStringList;
    FOptPlaylistExts:string;
    FstPlaylistExtList:TStringList;

    FOptFullscreen:boolean;
    FOptTransitEffect:boolean;
    FOptStretch:boolean;
    FOptFit:boolean;
    FOptExpand:boolean;
    FOptIntervalIntSeconds:integer;
    FOptMinimulFileSizeKiloByte:integer;
    FOptManualTransition:boolean;
    FOptRepeat:boolean;
    FOptRandom:boolean;

    FisFullScreen: boolean;
    FOrigBounds: TRect;
    FOrigWndState: TWindowState;

    FCurrentFileIndex:integer;

    procedure ShowFullScreen(AValue: boolean);
    procedure SetFullScreen_Universal(AValue: boolean);
    procedure SetFullScreen_Win32(AValue: boolean);
    procedure SetMoniter(intMoniter:integer);
    procedure RestoreFormState;
    procedure StoreFormState;
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
    property OptManualTransition: boolean read FOptManualTransition;
    property OptRandom: boolean read FOptRandom;
    property OptRepeat: boolean read FOptRepeat;

  end;

var
  frmMain: TfrmMain;

implementation

uses UFullscreen;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i,j:integer;
  folderfiles:TStringlist;
  fileSearchMask:string;
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
  FstMoniterList.Duplicates := dupIgnore;
  FstMoniterList.Sorted := true;


  //set defaults
  FOptFullscreen:=true;
  FOptTransitEffect:=true;
  FOptStretch:=false;
  FOptExpand:=false;
  FOptFit:=true;
  FOptIntMoniter:=0;
  FOptIntervalIntSeconds:=4;
  FOptMinimulFileSizeKiloByte:=1;
  fOptManualTransition:=true;
  FOptRandom:=true;
  FOptRepeat:=true;

  FOptFileExts:='.jpg;.jpeg;*.jpe;.png;.gif';
  FstFileExtList := TStringList.Create;
  FstFileExtList.Delimiter:=';';
  FstFileExtList.DelimitedText:=FOptFileExts;

  FOptPlaylistExts:='.imagelist';
  FstPlaylistExtList:= TStringList.Create;
  FstPlaylistExtList.Delimiter:=';';
  FstPlaylistExtList.DelimitedText:=FOptPlaylistExts;


  //parse prameter string.
  for I := 1 to ParamCount do
  begin
    if (AnsiStartsStr('-',ParamStr(I))) then
    begin
      //options

    end else if (FileExists(ParamStr(I))) then begin
      //found a file
      if (FstFileExtList.IndexOf(LowerCase(ExtractFileExt(ParamStr(I)))) >= 0) then
      begin
        //is picture file
        if not (AnsiStartsStr('.',ExtractFilename(ParamStr(I)))) then
        begin
          FstFileList.Add(ParamStr(I));
        end;
      end else if (FstPlaylistExtList.IndexOf(LowerCase(ExtractFileExt(ParamStr(I)))) >= 0) then
      begin
        //found a playlist
        FstPlaylistList.Add(ParamStr(I));
      end;
    end else if (DirectoryExists(ParamStr(I))) then begin
      //found a folder
      if not (AnsiStartsStr('.',ExtractFilename(ParamStr(I)))) then
      begin
        FstDirectoryList.Add(ParamStr(I));
      end
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
        folderfiles := FindAllFiles(FstDirectoryList[i], fileSearchMask, true); //'*.jpg;*.jpeg;*.png;'
        for j:=0 to folderfiles.Count - 1 do
        begin
          if not (AnsiStartsStr('.',ExtractFilename(folderfiles[j]))) then
          begin
            FstFileList.Add(folderfiles[j]);
          end;
        end;
      finally
        folderfiles.Free;
      end;
    end;
  end;

  //TODO playlist
  //open ploylist files and add them to the filelist.


  //TODO trayicon
  TrayIcon1.Hint:=self.Caption; //+ moniter[1]    etc
  //ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'');



  if (FstFileList.Count < 1) or Application.HasOption('h', 'help') then begin
    //No files are provided in the parameter string, so open "file open" dialog.
    //TODO modal dialog with options and playlist edit tab (drag & drop files) and "About" tab.

    OpenPictureDialog1.Title:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'')+' - Open picture file(s)';
    if OpenPictureDialog1.Execute then
    begin
      for i:=0 to OpenPictureDialog1.Files.Count -1 do
      begin
        //Application.ProcessMessages;
        if (FstFileExtList.IndexOf(LowerCase(ExtractFileExt(OpenPictureDialog1.Files[i]))) >= 0) then
        begin
          if not (AnsiStartsStr('.',ExtractFilename(OpenPictureDialog1.Files[i]))) then
          begin
            FstFileList.Add(OpenPictureDialog1.Files[i]);
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


  //clean up
  {
  j:=FstFileList.Count-1;
  for i:=j downto 0 do
  begin
    if (AnsiStartsStr('.',ExtractFilename(FstFileList[i]))) then
    begin
      FstFileList.Delete(i);
    end;
  end;
  }

  if (FstFileList.Count < 1) then begin
    //We tried. But there is nothing to do, so self terminate.
    Application.Terminate;
    exit;
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

  if Screen.MonitorCount > 1 then
  begin
    for i:=0 to Screen.MonitorCount-1 do
    begin
      FstMoniterList.Add(intToStr(i));
    end;
  end;


  //TODO i18n
  //http://wiki.lazarus.freepascal.org/Step-by-step_instructions_for_creating_multi-language_applications
  //http://wiki.lazarus.freepascal.org/Translations_/_i18n_/_localizations_for_programs

  //TODO change wallpaper
  //for unity and gnome3(gnome shell)
  // $ gsettings get org.gnome.desktop.background picture-uri
  // $ gsettings set org.gnome.desktop.background picture-uri "file:///home/user/background.jpg"
  // $ gnome-session --version
  //https://askubuntu.com/questions/66914/how-to-change-desktop-background-from-command-line-in-unity


  //load ini settings
  //TODO.
  XMLConfig.FileName:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'.xml');


  if FOptFullscreen then
  begin
    self.ShowInTaskBar:=stNever;
    self.Image1.Visible:=false;

    self.WindowState:=wsFullScreen;
    self.AlphaBlend:=true;
    self.AlphaBlendValue:=1;
    self.Show;
    self.BringToFront;
  end else
  begin
    //TODO
    self.Show;
    self.BringToFront;
  end;

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin

  if Application.Terminated then exit;

  if FOptFullscreen then
  begin
    Screen.Cursor:=crNone;
    Screen.UpdateScreen;

    {$ifdef windows}
    //self.BringToFront;
    //SetForegroundWindow(self.Handle);
    {$else}
    //does it need or work on linux?
    {$endif}
    ShowFullScreen(true);

    if FOptTransitEffect then
    begin
      self.AlphaBlendValue:=1;
      //start transition timer, and timer creates fullscreen
      TimerEffectStart.Enabled:=true;
    end else
    begin
      self.AlphaBlendValue := 255;
      //Create fullscreen form at "FormActivate"
    end;
  end else begin
    self.Position:=poDefault;//poScreenCenter;
    self.WindowState:=wsNormal;

    self.Top:=120;
    self.Left:=120;
    self.Width:=480;
    self.Height:=480;
    if fileexists(XMLConfig.FileName) then begin
       RestoreFormState;
    end;

    self.AlphaBlend:=true;
    self.AlphaBlendValue:=255;
    self.Image1.Visible:=true;

    FCurrentFileIndex:=0;

    if FileList.Count > 0 then begin
      Image1.Picture.LoadFromFile(FileList[FCurrentFileIndex]);
    end;

    SetForegroundWindow(self.Handle);
  end;
end;

procedure TfrmMain.TimerEffectStartTimer(Sender: TObject);
begin
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
    frmFullScreen.WindowState:=wsFullScreen;
    frmFullscreen.ShowModal;
    close; //when returned (frmFullscreen is closed), self close.
  end;
end;

procedure TfrmMain.Image1DblClick(Sender: TObject);
begin
  if self.WindowState = wsNormal then begin
    self.WindowState:=wsMaximized;
  end else if self.WindowState = wsMaximized then begin
    self.WindowState:=wsNormal;
  end;
end;

procedure TfrmMain.Image1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin

    if FCurrentFileIndex < FileList.Count -1 then begin
     Image1.Picture.LoadFromFile(FileList[FCurrentFileIndex+1]);
     FCurrentFileIndex:=FCurrentFileIndex+1;
     Handled:=true;
    end;
end;

procedure TfrmMain.Image1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin

    if FCurrentFileIndex > 0 then begin
     Image1.Picture.LoadFromFile(FileList[FCurrentFileIndex-1]);
     FCurrentFileIndex:=FCurrentFileIndex-1;
     Handled:=true;
    end;
end;


procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if Application.Terminated then exit;

  if FOptFullscreen then
  begin
    Screen.Cursor:=crNone;
    Screen.UpdateScreen;

    if not FOptTransitEffect then
    begin
      frmFullscreen := TfrmFullscreen.create(self);
      frmFullScreen.WindowState:=wsFullScreen;
      frmFullscreen.ShowModal;
      close; //when returned (frmFullscreen is closed), self close.
    end;
  end else begin
    //TODO
    {
    frmFullscreen := TfrmFullscreen.create(self);
    frmFullscreen.Position:=poOwnerFormCenter;
    frmFullScreen.Top:=self.Top;
    frmFullScreen.left:=self.left;
    frmFullScreen.width:=self.width;
    frmFullScreen.height:=self.height;
    frmFullScreen.WindowState:=wsNormal;

    frmFullscreen.ShowModal;
    close;
    }
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FOptFullscreen then
  begin
    //todo fullscreen options
  end else begin
    //saves normal form size and pos.
    StoreFormState;
  end;

end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  if FOptFullscreen then
  begin
    Screen.UpdateScreen;
    self.Cursor:=crNone;
    Screen.Cursor:=crNone;
    Screen.UpdateScreen;
  end;
end;

procedure TfrmMain.ApplicationProperties1Exception(Sender: TObject; E: Exception
  );
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

procedure TfrmMain.ApplicationProperties1Restore(Sender: TObject);
begin

end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FOptFullscreen and FisFullscreen then
  begin
    if ((Key = VK_F11) or (Key = VK_ESCAPE)) then
    begin
      close;
    end;
  end else begin
    //next
    if (Key = VK_RIGHT) then begin
      if FCurrentFileIndex < FileList.Count -1 then begin
       Image1.Picture.LoadFromFile(FileList[FCurrentFileIndex+1]);
       FCurrentFileIndex:=FCurrentFileIndex+1;
      end;
    end;
    //back
    if (Key = VK_LEFT) or (Key = VK_BACK) then begin
      if FCurrentFileIndex > 0 then begin
       Image1.Picture.LoadFromFile(FileList[FCurrentFileIndex-1]);
       FCurrentFileIndex:=FCurrentFileIndex-1;
      end;
    end;
  end;
end;

procedure TfrmMain.MenuItemQuitClick(Sender: TObject);
begin
  close;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FstFileExtList.Free;
  FstPlaylistExtList.Free;
  FstFileList.Free;
  FstDirectoryList.Free;
  FstPlaylistList.Free;
  //if Assigned(frmFullscreen) then frmFullscreen.Free;
end;

procedure TfrmMain.ShowFullScreen(AValue: boolean);
begin
  {$ifdef windows}
  SetFullScreen_Win32(AValue);
  {$else}
  SetFullScreen_Universal(AValue);
  {$endif}
  FisFullscreen:=AValue;
end;

procedure TfrmMain.SetFullScreen_Universal(AValue: boolean);
begin
  if AValue then
    ShowWindow(Handle, SW_SHOWFULLSCREEN)
  else
    ShowWindow(Handle, SW_SHOWNORMAL);
end;

procedure TfrmMain.SetFullScreen_Win32(AValue: boolean);
begin
  if AValue then
  begin
    FOrigWndState:= WindowState;
    FOrigBounds:= BoundsRect;
    BorderStyle:= bsNone;
    BoundsRect:= Screen.Monitors[FOptIntMoniter].BoundsRect; //Monitor.BoundsRect;

  end
  else
  begin
    WindowState:= FOrigWndState;
    BoundsRect:= FOrigBounds;
    BorderStyle:= bsSizeable;
    BoundsRect:= FOrigBounds; //again
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
    //TODO non fullscreen form.
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

