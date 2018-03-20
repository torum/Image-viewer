unit UFullscreen;

{$mode objfpc}{$H+}

{
Debug:
 DEFINE MyDebug
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LclType, LclProc, LclIntf, Menus
  {$ifdef MyDebug}, strutils{$endif}{$ifdef windows}, Windows{$endif};

type

  { TfrmFullscreen }

  TfrmFullscreen = class(TForm)
    IdleTimerMouseHide: TIdleTimer;
    Image1: TImage;
    MenuItemBorderForSOT: TMenuItem;
    MenuItemStart: TMenuItem;
    MenuItemStayOnTop: TMenuItem;
    MenuItemRepeat: TMenuItem;
    MenuItemRandom: TMenuItem;
    MenuItemFit: TMenuItem;
    MenuItemExpand: TMenuItem;
    MenuItemStretchBoth: TMenuItem;
    MenuItemMoniters: TMenuItem;
    MenuItemFilterFileSize: TMenuItem;
    MenuItemPlayback: TMenuItem;
    MenuItemInterval: TMenuItem;
    MenuItemFilter: TMenuItem;
    MenuItemQuit: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItemBack: TMenuItem;
    MenuItemNext: TMenuItem;
    MenuItemPause: TMenuItem;
    MenuItemEffect: TMenuItem;
    MenuItemStretch: TMenuItem;
    PopupMenu1: TPopupMenu;
    TimerInterval: TTimer;
    TimerFadeIn: TTimer;
    TimerFadeOut: TTimer;

    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure IdleTimerMouseHideTimer(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemStartClick(Sender: TObject);
    procedure MenuItemStayOnTopClick(Sender: TObject);
    procedure MenuItemExpandClick(Sender: TObject);
    procedure MenuItemFitClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemBackClick(Sender: TObject);
    procedure MenuItemNextClick(Sender: TObject);
    procedure MenuItemPauseClick(Sender: TObject);
    procedure MenuItemEffectClick(Sender: TObject);
    procedure MenuItemRandomClick(Sender: TObject);
    procedure MenuItemRepeatClick(Sender: TObject);
    procedure MenuItemStretchBothClick(Sender: TObject);
    procedure PopupMenu1Close(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure TimerFadeInTimer(Sender: TObject);
    procedure TimerFadeOutTimer(Sender: TObject);
    procedure TimerIntervalTimer(Sender: TObject);

  private
    FFileList:TStringlist;
    FFileListHistory:TStringlist;
    FFileListRemaining:TStringList;
    FiCurr:integer;
    FstrCurr:string;
    FInterval:integer;
    FStretch:boolean;
    FExpand:boolean;
    FFit:boolean;
    FEffect:boolean;
    FMinimulFileSizeKiloByte:integer;
    FManualTransition:boolean;
    FRandom:boolean;
    FRepeat:boolean;
    FIsFullScreen: boolean;
    FIsSlideshowPlaying:boolean;
    FOrigBounds: TRect;
    FOrigWndState: TWindowState;
    FiStartWith:integer;
    FOptIntMoniter:integer;
    FisInFrame :boolean;
    FisStartNormal:boolean;
    FXPos, FYPos: Integer;
    FisMoving: Boolean;
    FisPopupMenuShowing:boolean;

    procedure ShowFullScreen(blnOn: boolean);
    procedure SetFullScreen_Universal(blnOn: boolean);
    procedure SetFullScreen_Win32(blnOn: boolean);
    procedure StartSlideshow(startIndex:integer);
      {Starts slideshow begining of specified "starts" index.
      }
    function DisplayImage(id:integer):integer;
      {Display an Image.
        Takes an index of the playback list, return current index.
    }
    function ValidateFileIndex(id:integer):boolean;
      {Validates if given int is in range of the list.
        Returns true if id is in range of the playback list,
      }
    function ValideteFile(id:integer):integer;
      {Validate Image file. It determines if it is exists.
        Returns -1 if "not there", or return "FileSize".
      }
    procedure DisplayError(id:integer;message:string);
      {Takes an index of the playback list,
        Return current index.
      }
    function GetNextImageIndex(id:integer):integer;
      {Takes current index, and returns next index of the playback list.
        Returns -1 if there is no next or reached at the end of the list.
      }
    function GetNextRandomImageIndex():integer;
      {Takes current index, and returns next Random index of the playback list
        which does not exists in history list.
        Returns -1 if there is no next or reached at the end of the list.
      }
    function GetPreviousImageIndex(id:integer):integer;
      {Takes current index, and returns previous index of the playback list.
        Returns -1 if there is no previous or reached at the beggining.
      }
    procedure ChangeMoniterClicked(Sender: TObject);
    procedure ChangeIntervalClicked(Sender: TObject);
    procedure ChangeMinFileSizeClicked(Sender: TObject);
    procedure PlaybackRepeatStart();
    procedure ResizeImage();
  public
    procedure PlaybackNext();
    procedure PlaybackBack();
    procedure PlaybackPause();
    procedure PlaybackPlay(preLoad:boolean);
    property Current: integer read FiCurr;
    property StartWith: integer write FiStartWith;
  end;

var
  frmFullscreen: TfrmFullscreen;

implementation

uses UMain;

{$R *.lfm}


procedure TfrmFullscreen.FormCreate(Sender: TObject);
var
  i:integer;
  childItem: TMenuItem;
begin

  FiCurr:=-1;
  {$ifdef windows}
  TimerFadeIn.Interval:=1;
  TimerFadeOut.Interval:=2;
  {$else}
  TimerFadeIn.Interval:=10;
  TimerFadeOut.Interval:=12;
  {$endif}

  FFileList:=TStringlist.Create;
  FFileList.Assign(frmMain.FileList);
  FFileListHistory:=TStringList.Create;
  FFileListRemaining:=TStringlist.Create;

  FInterval:= frmMain.OptIntervalIntSeconds*1000;
  TimerInterval.Interval:=FInterval;
  TimerInterval.Enabled:=false;

  FStretch:= frmMain.OptStretch;
  FFit:=frmMain.OptFit;
  FExpand:=frmMain.OptExpand;
  FEffect:=frmMain.OptTransitEffect;
  FMinimulFileSizeKiloByte:=frmMain.OptMinimulFileSizeKiloByte;
  FManualTransition:=frmMain.IsManualTransition;
  FRandom:= frmMain.OptRandom;
  FRepeat:= frmMain.OptRepeat;

  FOptIntMoniter:=frmMain.OptIntMoniter;
  FisInFrame:=frmMain.IsInFrame;
  FisStartNormal:=frmMain.IsStartNormal;

  FiStartWith:=0;

  Image1.Stretch:=false;
  if FStretch or FFit then
    Image1.StretchInEnabled:=true
  else
    Image1.StretchInEnabled:=false;
  Image1.StretchOutEnabled:=true;
  Image1.Proportional:=true;
  Image1.Center:=true;

  // i18n

  MenuItemPlayback.Caption:=resstrPlayback;
  MenuItemNext.Caption:=resstrPlaybackNext;
  MenuItemBack.Caption:=resstrPlaybackPrevious;
  MenuItemPause.Caption:=resstrPlaybackPause;
  MenuItemInterval.Caption:=resstrInterval;
  MenuItemRandom.Caption:=resstrRandom;
  MenuItemRepeat.Caption:=resstrRepeat;
  MenuItemEffect.Caption:=resstrEffect;
  MenuItemStretch.Caption:=resstrStretch;
  MenuItemFit.Caption:=resstrStretchIn;
  MenuItemExpand.Caption:=resstrStretchOut;
  MenuItemFilter.Caption:=resstrFilter;
  MenuItemFilterFileSize.Caption:=resstrFilterFileSize;
  MenuItemMoniters.Caption:=resstrMoniters;
  MenuItemQuit.Caption:=resstrQuit;
  MenuItemStayOnTop.Caption:=resstrStayOnTop;

  // Popup Menues

  childItem := TMenuItem.Create(PopupMenu1);
  childItem.Caption := '[&1] ' + resstrSecond;
  childItem.OnClick := @ChangeIntervalClicked;
  childItem.Tag:=1;
  MenuItemInterval.Add(childItem);

  for i:=1 to 9 do begin
    childItem := TMenuItem.Create(PopupMenu1);
    childItem.Caption := '[&'+intToStr(i+1)+'] ' + resstrSeconds;
    childItem.OnClick := @ChangeIntervalClicked;
    childItem.Tag:=i+1;
    MenuItemInterval.Add(childItem);
  end;
  childItem := TMenuItem.Create(PopupMenu1);
  childItem.Caption := '[1] ' + resstrMinute;
  childItem.OnClick := @ChangeIntervalClicked;
  childItem.Tag:=60;
  MenuItemInterval.Add(childItem);
  childItem := TMenuItem.Create(PopupMenu1);
  childItem.Caption := '[5] ' + resstrMinutes;
  childItem.OnClick := @ChangeIntervalClicked;
  childItem.Tag:=300;
  MenuItemInterval.Add(childItem);
  childItem := TMenuItem.Create(PopupMenu1);
  childItem.Caption := '[10] ' + resstrMinutes;
  childItem.OnClick := @ChangeIntervalClicked;
  childItem.Tag:=600;
  MenuItemInterval.Add(childItem);
  childItem := TMenuItem.Create(PopupMenu1);
  childItem.Caption := '[15] ' + resstrMinutes;
  childItem.OnClick := @ChangeIntervalClicked;
  childItem.Tag:=900;
  MenuItemInterval.Add(childItem);

  // Minimum file size
  childItem := TMenuItem.Create(PopupMenu1);
  childItem.Caption := '> [&'+intToStr(1)+'] KB';
  childItem.OnClick := @ChangeMinFileSizeClicked;
  childItem.Tag:=1;
  MenuItemFilterFileSize.Add(childItem);
  childItem := TMenuItem.Create(PopupMenu1);
  childItem.Caption := '> [&'+intToStr(50)+'] KB';
  childItem.OnClick := @ChangeMinFileSizeClicked;
  childItem.Tag:=50;
  MenuItemFilterFileSize.Add(childItem);
  for i:=0 to 9 do
  begin
    childItem := TMenuItem.Create(PopupMenu1);
    childItem.Caption := '> [&'+intToStr(i+1)+'00] KB';
    childItem.OnClick := @ChangeMinFileSizeClicked;
    childItem.Tag:=i*100;
    MenuItemFilterFileSize.Add(childItem);
  end;

  // Moniters
  for i:=0 to Screen.MonitorCount-1 do
  begin
    childItem := TMenuItem.Create(PopupMenu1);
    childItem.Caption := resstrMoniter + ' [&'+intToStr(i+1)+']';
    childItem.OnClick := @ChangeMoniterClicked;
    childItem.Tag:=i;
    MenuItemMoniters.Add(childItem);
  end;

  self.Visible:=false; // Don't set to true.
  self.ShowInTaskBar:=stNever;

  self.AlphaBlend:=true;
  if FEffect then
  begin
    // do NOT ever set it to 0.
    self.AlphaBlendValue := 1;
  end else
  begin
    self.AlphaBlendValue := 255;
  end;

  // Important.
  Randomize;
end;

procedure TfrmFullscreen.FormShow(Sender: TObject);
begin
  // Changing moniter (show) cause slideshow to star over, so...
  if FIsSlideshowPlaying then exit;
  if FIsFullscreen then exit;

  if (FFileList.Count > 0) then
  begin
    // Sets fullscreen and show.
    if FisInFrame then
    begin
      self.BoundsRect := frmMain.ClientRect;
      StartSlideshow(FiStartWith);
    end else
    begin
      // Make it fullscreen.
      ShowFullScreen(true);
      // Start.
      StartSlideshow(FiStartWith);
    end;
  end else begin
    // No files are selected or to open. List empty.
    // This should not happen.
  end;
end;

procedure TfrmFullscreen.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if FisInFrame then
  begin
    TimerInterval.Enabled:=false;
    TimerFadeOut.Enabled:=false;
    TimerFadeIn.Enabled:=false;
  end;
  CanClose:=true;
end;

procedure TfrmFullscreen.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;

  TimerInterval.Enabled := false;
  TimerFadeIn.Enabled := false;
  TimerFadeOut.Enabled := false;

  IdleTimerMouseHide.Enabled:=false;
  Screen.Cursor:=crDefault;

  // Apply chagens in options.
  frmMain.OptTransitEffect := FEffect;
  frmMain.OptFit := FFit;
  frmMain.OptExpand := FExpand;
  frmMain.OptIntervalIntSeconds := FInterval div 1000;
  frmMain.OptRandom := FRandom;
  frmMain.OptRepeat := FRepeat;

  if FisInFrame then
  begin
    frmMain.DoneInFrame(FstrCurr);
  end else begin
    frmMain.DoneFullscreen(FstrCurr);
  end;

  //ShowFullScreen(false);
end;

procedure TfrmFullscreen.FormDestroy(Sender: TObject);
begin
  FFileList.Free;
  FFileListHistory.Free;
  FFileListRemaining.Free;
end;

procedure TfrmFullscreen.StartSlideshow(startIndex:integer);
begin
  // Reset
  FFileListHistory.Clear;
  FFileListRemaining.Clear;
  FFileListRemaining.Assign(FFileList);

  FIsSlideshowPlaying:=true;

  // Specify index to display.
  if FRandom then begin
    if startIndex > -1 then
    begin
      TimerInterval.tag:= startIndex;
    end else
    begin
      TimerInterval.tag:=GetNextRandomImageIndex();
    end;
  end else
  begin
    TimerInterval.tag:=startIndex;
  end;

  if (not frmMain.OptSlideshowAutoStart) or frmMain.IsSingleFileSelected then
  begin
    // Did not add files manually. started as single file. So,
    FManualTransition:=true;
  end else
  begin
    FManualTransition:=false;
  end;

  // Start slideshow.
  PlaybackPlay(false);

end;

procedure TfrmFullscreen.PlaybackPlay(preLoad:boolean);
var
  i,iNext:integer;
begin

  //TODO if preLoad,

  TimerInterval.Enabled:=false; //just in case.

  {$ifdef Mydebug}
  OutputDebugString(PChar(TrimRight( 'PlaybackPlay:id '+ intToStr(TimerInterval.Tag) )));
  {$endif}

  i:=TimerInterval.Tag;
  if ValidateFileIndex(i) then
  begin

    // Display image
    FiCurr:=DisplayImage(i);
    FstrCurr:=FFileList[FiCurr];

    if FiCurr < 0 then
    begin
      //TODO:
      // Skipping or error
      //DisplayError(i,'file skipping. error   .'+intToStr(i));

      // If you set Repeat and FileSize Filter, it triggers infinite loop.

    end else
    begin
      if FisInFrame then
      begin
        if (FFileList.Count = 1) then
        begin
          frmMain.SetCaption('InFrame Slideshow - ' + FFileList[FiCurr]);
        end else
        begin
          frmMain.SetCaption('InFrame Slideshow: '+'['+intToStr(FiCurr+1)+'/'+ intToStr(FFileList.Count) +'] - ' + FFileList[FiCurr]);
        end;
      end;
    end;

  end else begin
    DisplayError(i,'Display file index out of rainge.'+intToStr(i));
    {$ifdef Mydebug}
    OutputDebugString(PChar(TrimRight( 'Display file index out of rainge.'+intToStr(i) )));
    {$endif}
    exit;
  end;

  // Next cycle.
  if FFileList.Count > 0 then
  begin

    // Tell Timer what to play.
    if FRandom then begin
      iNext:=GetNextRandomImageIndex();
    end else begin
      iNext:=GetNextImageIndex(Current);
    end;

    if (iNext > -1) then
    begin
      {$ifdef Mydebug}
      OutputDebugString(PChar(TrimRight( 'PlaybackPlay: ->Next:'+ intToStr(iNext))));
      {$endif}
      // Set next
      TimerInterval.tag:= iNext;

      // Start timer
      if FEffect then begin
        TimerFadeIn.Enabled:=true;
      end else begin
        TimerInterval.Enabled:=true;
      end;

    end else
    begin
      // Next is the end of list.
      {$ifdef Mydebug}
      OutputDebugString(PChar(TrimRight( 'PlaybackPlay: ->PlaybackRepeatStart')));
      {$endif}
      if FEffect then begin
        // It's the last but you need to fade in.
        // TimerFadeIn checks repeat option.
        TimerFadeIn.Enabled:=true;

      end else begin
        // Checking repeat option.
        PlaybackRepeatStart();
      end;
    end;
  end;
end;

procedure TfrmFullscreen.PlaybackRepeatStart();
begin

  {$ifdef Mydebug}
  OutputDebugString(PChar(TrimRight( 'PlaybackRepeatStart ')));
  {$endif}

  if FRepeat then begin
    // Start from beggining.

    // Clear
    FFileListRemaining.Clear;
    FFileListRemaining.Assign(FFileList);

    // Reset next
    if FRandom then begin
      FiCurr:=-1;
      TimerInterval.tag:=GetNextRandomImageIndex();
    end else begin
      FiCurr:=0;
      TimerInterval.tag:=0;
    end;

    {$ifdef Mydebug}
    OutputDebugString(PChar(TrimRight( 'PlaybackRepeatStart '+ intToStr(TimerInterval.tag) )));
    {$endif}

    // Start timer
    TimerInterval.Enabled:=true;

  end else begin
    //TODO: self terminate?
    {$ifdef Mydebug}
    OutputDebugString(PChar(TrimRight( 'PlaybackRepeatStart: No repeat.')));
    {$endif}
  end;
end;

procedure TfrmFullscreen.PlaybackPause();
begin
  // Toggle playback pause/start
  if FIsSlideshowPlaying then begin

    if FManualTransition then
    begin
      FManualTransition:=false;

      if TimerFadeIn.Enabled then
      begin
        TimerInterval.Enabled:=true;
      end else
      begin
        if FEffect then
        begin
          TimerFadeOut.Enabled:=true;
        end else
        begin
          PlaybackPlay(false);
        end;
      end;

    end else
    begin
      FManualTransition:=true;

      TimerInterval.Enabled:=false;
      TimerFadeIn.Enabled:=false;
      TimerFadeOut.Enabled:=false;

      self.AlphaBlendValue:=255;
    end;

  end;
end;

procedure TfrmFullscreen.PlaybackNext();
begin
  if FFileList.Count > 1 then begin

    TimerInterval.Enabled:=false;

    if FEffect then begin
      if TimerFadeIn.Enabled then
      begin
        // Fading IN. so just speed up.
        self.AlphaBlendValue:=255;
        TimerFadeInTimer(nil);
        TimerFadeOut.Enabled:=true;
      end else if TimerFadeOut.Enabled then
      begin
        // Fading OUT. so let's just speed up.
        self.AlphaBlendValue:=1;
        TimerFadeOutTimer(nil);
      end else
      begin
        TimerFadeOut.Enabled:=true;
      end;

    end else begin
      PlaybackPlay(false);
    end;

  end;
end;

procedure TfrmFullscreen.PlaybackBack();
var
  iPrev:integer;
begin
  if FFileList.Count > 1 then begin
    //TODO: when you go back, put current to remaining?

    iPrev:=GetPreviousImageIndex(Current);

    if (iPrev > -1) then
    begin
      if FEffect then begin
        if TimerFadeIn.Enabled then
        begin
          TimerFadeIn.Enabled:=false;
          alphablendvalue:=255;
          TimerInterval.Enabled:=false;
          TimerInterval.Tag := iPrev;
          PlaybackPlay(false);
        end else if TimerFadeOut.Enabled then
        begin
          // Let's just go back to the picture we are watching.
          TimerFadeOut.Enabled:=false;
          TimerInterval.Enabled:=false;
          alphablendvalue:=255;
          TimerInterval.Enabled:=true;
        end else
        begin
          TimerInterval.Enabled:=false;
          TimerInterval.Tag := iPrev;
          alphablendvalue:=255;
          TimerFadeOut.Enabled:=false;
          TimerInterval.Enabled:=false;
          PlaybackPlay(false);
        end;

      end else begin
        TimerInterval.Enabled:=false;
        TimerInterval.Tag := iPrev;
        PlaybackPlay(false);
      end;
    end;

  end;
end;

function TfrmFullscreen.ValidateFileIndex(id:integer):boolean;
begin
  if (id > -1) and ((FFileList.Count-1) >= id) then
  begin
    result := true;
  end else
  begin
    result := false;
  end;
end;

function TfrmFullscreen.ValideteFile(id:integer):integer;
begin
  // FileExists?
  result := FileSize(FFileList[id]);
end;

procedure TfrmFullscreen.ResizeImage();
var
  curWidth,curHeight:integer;
begin
  if FisInFrame then
  begin
    curWidth := self.Parent.ClientWidth;
    curHeight:= self.Parent.ClientHeight;
  end else
  begin
    curWidth := screen.Monitors[FOptIntMoniter].Width;
    curHeight:= screen.Monitors[FOptIntMoniter].Height;
  end;

  if FStretch then begin
     Image1.Stretch:=true;
  end else begin
    if FFit then begin
      if ((Image1.Picture.Width > curWidth) or
              (Image1.Picture.height > curHeight)) then begin
        Image1.Stretch:=true;
        Image1.StretchInEnabled:=true;
        // Fit only when larger than screen size.
      end else begin
        Image1.Stretch:=false;
        Image1.StretchInEnabled:=false;
      end;
    end else begin
      Image1.Stretch:=false;
    end;
    if FExpand then begin
      Image1.Stretch:=true;
    end;
  end;
end;

function TfrmFullscreen.DisplayImage(id:integer):integer;
var
  f:integer;
begin
  f:= ValideteFile(id);
  if f >= (FMinimulFileSizeKiloByte * 1024) then
  begin

    Image1.Picture.Clear;
    Image1.Stretch:=false;
    Image1.StretchInEnabled:=false;
    {
    if FisInFrame then
    begin
      curWidth := self.Parent.ClientWidth;
      curHeight:= self.Parent.ClientHeight;
    end else
    begin
      curWidth := screen.Monitors[FOptIntMoniter].Width;
      curHeight:= screen.Monitors[FOptIntMoniter].Height;
    end;
    }

    try
      Image1.Picture.LoadFromFile(FFileList[id]);

      ResizeImage();

      Image1.Refresh;
      FFileListHistory.Add(FFileList[id]);
      result:=id;
      {$ifdef Mydebug}
      OutputDebugString(PChar(TrimRight( 'DisplayImage:id '+ intToStr(id) )));
      {$endif}
    except
      On E :Exception do begin
        {$ifdef Mydebug}
        OutputDebugString(PChar(TrimRight( 'Exception@DisplayImage_LoadFromFile:id '+ intToStr(id)+':'+E.Message )));
        {$endif}
        Image1.Picture.Clear;
        DisplayError(id,'DisplayImage@LoadFromFile - '+E.ClassName+' - '+E.Message);

        // On error, stop.
        //result:=-1;

        // On error, keep going.
        result:=id;
      end;
    end;
  end else
  begin
    // File does not exists.
    if (f < 0) then begin
      Image1.Picture.Clear;
      self.AlphaBlendValue:=255;
      with image1.Canvas do
        begin
          Brush.Style := bsClear;
          Font.Color := clWhite;
          TextOut(24,24, 'File not found: '+FFileList[id]);
      end;
      Image1.Update;
      result:=id;
      {$ifdef Mydebug}
      OutputDebugString(PChar(TrimRight( 'DisplayImage FileNotFound:id '+ intToStr(id) )));
      {$endif}
    end else begin
      // File size just too small. File size became smaller during the slideshow.
      self.AlphaBlendValue:=255;
      Image1.Picture.Clear;
      with image1.Canvas do
        begin
          Brush.Style := bsClear;
          Font.Color := clWhite;
          TextOut(24,24, 'File size is wrong: '+FFileList[id]);
      end;
      Image1.Update;
      result:=id;

      //TODO: getNext and continue?
      //result:=-1;
    end;
  end;

  if (FFileListRemaining.IndexOf(FFileList[id]) > -1) then begin
     FFileListRemaining.Delete(FFileListRemaining.IndexOf(FFileList[id]));
  end;
end;


procedure TfrmFullscreen.DisplayError(id:integer;message:string);
var
  strFilePath:string;
  {$ifdef MyDebug} esl:TStringlist;
  logFile:string; {$endif}
begin
  if ValidateFileIndex(id) then
  begin
   strFilePath:=FFileList[id];
  end;

  Image1.Picture.Clear;
  with image1.Canvas do
    begin
      Brush.Style := bsClear;
      Font.Color := clWhite;
      TextOut(24,24, 'File load error: ' + message);
      TextOut(52,52, 'File: ' + strFilePath);
  end;
  Image1.Update;

  {$ifdef MyDebug}

  try
    esl:=TStringlist.Create;
    try
    logFile:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'.log');
    if FileExists(logFile) then
    begin
      esl.LoadFromFile(logFile);
    end;

    esl.Add(DateTimeToStr(Now));
    esl.Add('DisplayError: '+message);
    esl.Add('FileIndex: '+intToStr(id));
    esl.Add('File: '+strFilePath);
    esl.SaveToFile(logFile);

    finally
      esl.Free;
    end;
  except
    //
  end;

  {$endif}

end;

procedure TfrmFullscreen.TimerFadeInTimer(Sender: TObject);
var
  iNext:integer;
begin
  if FFileList.Count = 0 then
  begin
    TimerInterval.Enabled:=false;
    self.AlphaBlendValue := 255;
    //close;
    exit;
  end;

  if self.AlphaBlendValue < 255 then
  begin
    if (self.AlphaBlendValue < 100 ) then begin
       self.AlphaBlendValue := self.AlphaBlendValue + 10;
    end else
    begin
      if (self.AlphaBlendValue +5 >= 255) then begin
        self.AlphaBlendValue := 255;
      end else begin
        self.AlphaBlendValue := self.AlphaBlendValue + 4;
      end;
    end;
  end else begin

    TimerFadeIn.Enabled:=false;

    {$ifdef Mydebug}
    OutputDebugString(PChar(TrimRight( 'TimerFadeInTimer' )));
    {$endif}

    // Start timer or repeat check.
    if FRandom then begin
      iNext:=GetNextRandomImageIndex();
    end else begin
      iNext:=GetNextImageIndex(Current);
    end;

    if (iNext > -1) then begin
      // Start interval timer
      TimerInterval.Enabled:=true;
      //TODO: preLoading.

    end else begin
      PlaybackRepeatStart();
    end;

  end;
end;

procedure TfrmFullscreen.TimerIntervalTimer(Sender: TObject);
begin
  if FManualTransition then
  begin
    TimerInterval.Enabled:=false;
    exit;
  end;

  if FEffect then begin
    TimerFadeOut.Enabled:=true;
  end else begin
    PlaybackPlay(false);
  end;
end;


procedure TfrmFullscreen.TimerFadeOutTimer(Sender: TObject);
begin
  if FFileList.Count = 0 then
  begin
    TimerInterval.Enabled:=false;
    TimerFadeOut.Enabled:=false;
    //close;
    exit;
  end else if FFileList.Count = 1 then
  begin
    TimerInterval.Enabled:=false;
    TimerFadeOut.Enabled:=false;
    exit;
  end;

  TimerInterval.Enabled:=false;

  if self.AlphaBlendValue > 1 then
  begin
    if (self.AlphaBlendValue > 128 ) then begin
      self.AlphaBlendValue := self.AlphaBlendValue - 4;
    end else
    begin
      if (self.AlphaBlendValue  < 17) then begin
        self.AlphaBlendValue := 1;
      end else begin
        self.AlphaBlendValue := self.AlphaBlendValue - 15;
      end;
    end;
  end else begin
    TimerFadeOut.Enabled:=false;
    // Display.
    {$ifdef Mydebug}
    OutputDebugString(PChar(TrimRight( 'TimerFadeOutTimer: ->PlaybackPlay' )));
    {$endif}
    PlaybackPlay(false);
    {$ifdef Mydebug}
    OutputDebugString(PChar(TrimRight( 'TimerFadeOutTimer: ->(PlaybackPlay)TimerFadeIn' )));
    {$endif}

  end;
end;

function TfrmFullscreen.GetNextImageIndex(id:integer):integer;
begin
  if ValidateFileIndex(id) then begin
    if (id = (FFileList.Count-1)) then
    begin
      // End of the list. There is no NEXT.
      result:=-1;
    end else if (id > (FFileList.Count-1)) then
    begin
      //TODO: close? or ...
      result:=-1;
    end else begin
      // Just next.
      result:=id+1;
    end;
  end else
  begin
    // Something went wrong.
    //OutputDebugString(PChar(TrimRight('GetNextImageIndex:(id): ValidateFileIndex FAILED: '+intToStr(id))));
    result:=-1;
  end;
end;

function TfrmFullscreen.GetNextRandomImageIndex():integer;
var
  r:integer;
begin
  result:=-1;
  if FFileList.Count = 0 then begin
    exit;
  end;
  if (FFileListRemaining.Count = 0) then
  begin
    exit;
  end;
  if FFileListRemaining.Count = 1 then
  begin
    result:=FFileList.IndexOf(FFileListRemaining[0]);
    exit;
  end;
  r:= Random(FFileListRemaining.Count-1);
  result:=FFileList.IndexOf(FFileListRemaining[r]);
end;

function TfrmFullscreen.GetPreviousImageIndex(id:integer):integer;
begin
  if FRandom then
  begin
    if FFileListHistory.Count > 0 then begin

      //TODO: check. when you go back, put current to remaining,

      // Saving for the "next"
      FFileListRemaining.Insert(0,FFileListHistory[FFileListHistory.Count-1]);
      // Delete current?
      if ((FFileListHistory.Count-1) > 0) then
      begin
        FFileListHistory.Delete(FFileListHistory.Count-1);
      end;
      result := FFileList.IndexOf(FFileListHistory[FFileListHistory.Count-1]);
      // Delete for the future?
      if ((FFileListHistory.Count-1) > 0) then
      begin
        FFileListHistory.Delete(FFileListHistory.Count-1);
      end;
    end else begin
      result:=id;
    end;
  end else
  begin
    if ValidateFileIndex(id) then begin
      if (id <= 0) then
      begin
        // Beginning of the list. There is no PREVIOUS.
        result:=-1;
      end else
      begin
        result:=id-1;
      end;
    end else
    begin
      // Something went wrong.
      //OutputDebugString(PChar(TrimRight('GetNextImageIndex:(id): VALIDATE: '+intToStr(id))));
      result:=-1;
    end;
  end;
end;

procedure TfrmFullscreen.Image1DblClick(Sender: TObject);
begin
  //ShowFullScreen(false);
  close;
end;

procedure TfrmFullscreen.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FisInFrame then
  begin
    FXPos:=X;
    FYPos:=Y;
    FisMoving:=True;
  end;
end;

procedure TfrmFullscreen.Image1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Screen.Cursor:= crDefault;
  if FisInFrame then
  begin
    If FisMoving then frmMain.Left:=frmMain.Left+X-FXPos;
    If FisMoving then frmMain.Top:=frmMain.Top+Y-FYPos;
  end;
end;

procedure TfrmFullscreen.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FisInFrame then
  begin
    FisMoving:=False;
  end;
  if (Button = mbRight) then
  begin
    //PopupMenu1.PopUp; //don't
  end;
end;

procedure TfrmFullscreen.MenuItemStartClick(Sender: TObject);
begin
  FManualTransition:=false;
  TimerInterval.Enabled:=true;
end;

procedure TfrmFullscreen.MenuItemStayOnTopClick(Sender: TObject);
begin
  if FisInFrame then
  begin
    if frmMain.OptStayOnTopInframe then
    begin
      frmMain.OptStayOnTopInframe:=false;
    end else begin
      frmMain.OptStayOnTopInframe:=true;
    end;
  end;
end;

procedure TfrmFullscreen.MenuItemExpandClick(Sender: TObject);
begin
  if FStretch then FStretch:=false;
  if FExpand then begin
     FExpand :=false;
  end else begin
     FExpand :=true;
  end;

  if not Assigned(Image1.Picture) then exit;
  Image1.Stretch:=FExpand;
  Image1.Refresh;
end;

procedure TfrmFullscreen.MenuItemFitClick(Sender: TObject);
begin
  if FStretch then FStretch:=false;
  if FFit then begin
     FFit :=false;
     Image1.StretchInEnabled:=false;
  end else begin
     FFit :=true;
     Image1.StretchInEnabled:=true;
  end;

  if not Assigned(Image1.Picture) then exit;
  Image1.Refresh;
end;

procedure TfrmFullscreen.MenuItemStretchBothClick(Sender: TObject);
begin
  if FFit then FFit:=false;
  if FExpand then FExpand:=false;
  if FStretch then begin
     FStretch :=false;
     Image1.StretchInEnabled:=false;
  end else begin
     FStretch :=true;
     Image1.StretchInEnabled:=true;
  end;
  Image1.Stretch:=FStretch;
  Image1.Refresh;
end;

procedure TfrmFullscreen.PopupMenu1Close(Sender: TObject);
begin
  FisPopupMenuShowing:=false;
end;

procedure TfrmFullscreen.MenuItemQuitClick(Sender: TObject);
begin
  //ShowFullScreen(false);
  close;
end;

procedure TfrmFullscreen.MenuItemBackClick(Sender: TObject);
begin
  PlaybackBack();
end;

procedure TfrmFullscreen.MenuItemNextClick(Sender: TObject);
begin
  PlaybackNext();
end;

procedure TfrmFullscreen.MenuItemPauseClick(Sender: TObject);
begin
  PlaybackPause();
end;

procedure TfrmFullscreen.MenuItemEffectClick(Sender: TObject);
begin
  // Consider using timers.
  if FEffect then begin
    if TimerFadeOut.Enabled then begin
       TimerFadeOut.Enabled:=false;
       PlaybackPlay(false);
    end;
    if TimerFadeIn.Enabled then begin

    end;
    self.AlphaBlendValue:=255;
    FEffect:=false;
  end else begin
    FEffect:=true;
  end;
end;

procedure TfrmFullscreen.MenuItemRandomClick(Sender: TObject);
begin
  if FRandom then
  begin
    FRandom:=false;
    // Play first image.
    TimerInterval.tag:=0;
  end else
  begin
    FRandom:=true;
  end;
end;

procedure TfrmFullscreen.MenuItemRepeatClick(Sender: TObject);
begin
  if FRepeat then begin
    FRepeat:=false;
  end else begin
    FRepeat:=true;

    //TODO: Gotta be a better way to check if it's the end of list...
    if (not TimerInterval.Enabled) and
    (not TimerFadeIn.Enabled) and
    (not TimerFadeOut.Enabled) then
    begin
      PlaybackRepeatStart();
    end;
  end;
end;

procedure TfrmFullscreen.PopupMenu1Popup(Sender: TObject);
var
  i:integer;
begin
  FisPopupMenuShowing:=true;

  for i:=0 to MenuItemInterval.Count-1 do
  begin
    if (MenuItemInterval.Items[i].Tag * 1000) = FInterval then
    begin
      MenuItemInterval.Items[i].Checked:=true;
    end else
    begin
      MenuItemInterval.Items[i].Checked:=false;
    end;
  end;

  for i:=0 to MenuItemFilterFileSize.Count-1 do
  begin
    if MenuItemFilterFileSize.Items[i].Tag = FMinimulFileSizeKiloByte then
    begin
      MenuItemFilterFileSize.Items[i].Checked:=true;
    end else
    begin
      MenuItemFilterFileSize.Items[i].Checked:=false;
    end;
  end;

  if FStretch then
  begin
    MenuItemStretchBoth.checked:=true;
  end else
  begin
    MenuItemStretchBoth.checked:=false;
  end;

  if Ffit then
  begin
    MenuItemFit.checked:=true;
  end else
  begin
    MenuItemFit.checked:=false;
  end;

  if FExpand then
  begin
    MenuItemExpand.checked:=true;
  end else
  begin
    MenuItemExpand.checked:=false;
  end;

  if FEffect then
  begin
    MenuItemEffect.checked:=true;
  end else
  begin
    MenuItemEffect.checked:=false;
  end;

  if FRepeat then
  begin
    MenuItemRepeat.Checked:=true;
  end else
  begin
    MenuItemRepeat.Checked:=false;
  end;

  if FRandom then
  begin
    MenuItemRandom.Checked:=true;
  end else
  begin
    MenuItemRandom.Checked:=false;
  end;

  if screen.MonitorCount <= 1 then
  begin
    MenuItemMoniters.Visible:=false;
  end else
  begin
    if FFileList.Count >= 1 then
    begin
      MenuItemMoniters.Visible:=true;
      for i:=0 to MenuItemMoniters.Count-1 do
      begin
        if MenuItemMoniters.Items[i].Tag=FoptIntMoniter then
        begin
          MenuItemMoniters.Items[i].Checked:=true;
        end else
        begin
          MenuItemMoniters.Items[i].Checked:=false;
        end;
      end;
    end else
    begin
      MenuItemMoniters.Visible:=false;
    end;
  end;

  if FisInFrame then
  begin
    MenuItemMoniters.Visible:=false;
  end;

  if FisInFrame then
  begin
    if FFileList.Count > 1 then
    begin
      if FManualTransition then
      begin
        MenuItemQuit.Caption:=resstrLeaveInFrame;
      end else
      begin
        MenuItemQuit.Caption:=resstrStopInFrameSlideshow;
      end;
    end else
    begin
      MenuItemQuit.Caption:=resstrLeaveInFrame;
    end;
  end else if FisFullscreen then
  begin
    if FFileList.Count > 1 then
    begin
      if FisStartNormal then
      begin
        if FManualTransition then
        begin
          MenuItemQuit.Caption:=resstrLeaveFullscreen;
        end else
        begin
          MenuItemQuit.Caption:=resstrStopFullscreenSlideshow;
        end;
      end else begin
        MenuItemQuit.Caption:=resstrQuit;
      end;
    end else
    begin
      if FisStartNormal then
      begin
        MenuItemQuit.Caption:=resstrLeaveFullscreen;
      end else begin
        MenuItemQuit.Caption:=resstrClose;
      end;
    end;
  end;

  if FisInFrame then
  begin
    MenuItemBorderForSOT.Visible:=true;
    MenuItemStayOnTop.Visible:=true;
    if frmMain.OptStayOnTopInframe then
    begin
      MenuItemStayOnTop.Checked:=true;
    end else
    begin
      MenuItemStayOnTop.Checked:=false;
    end;
  end else
  begin
    MenuItemBorderForSOT.Visible:=false;
    MenuItemStayOnTop.Visible:=false;
  end;

  if (TimerInterval.Enabled or TimerFadeIn.Enabled or TimerFadeOut.Enabled) then
  begin
    MenuItemPause.Checked:=false;
  end else
  begin
    MenuItemPause.Checked:=true;
  end;
  if FManualTransition then
  begin
    MenuItemPause.Checked:=true;
  end;

  if FFileList.Count > 1 then
  begin
    MenuItemPlayback.Visible:=true;
    MenuItemRandom.Visible:=true;
    MenuItemRepeat.Visible:=true;
    MenuItemEffect.Visible:=true;
    MenuItemInterval.Visible:=true;
    //MenuItemFilter.Visible:=true;
  end else begin
    MenuItemPlayback.Visible:=false;
    MenuItemRandom.Visible:=false;
    MenuItemRepeat.Visible:=false;
    MenuItemEffect.Visible:=false;
    MenuItemInterval.Visible:=false;
    //MenuItemFilter.Visible:=false;
  end;

  if FFileList.Count <= 0 then
  begin
    MenuItemStretch.Visible:=false;
  end else
  begin
    MenuItemStretch.Visible:=true;
  end;

end;

procedure TfrmFullscreen.ChangeMoniterClicked(Sender: TObject);
begin

  FOptIntMoniter:= TMenuItem(Sender).Tag;

  frmMain.OptIntMoniter:= TMenuItem(Sender).Tag;

  ShowFullScreen(FisFullScreen);

end;

procedure TfrmFullscreen.ChangeIntervalClicked(Sender: TObject);
begin
  Finterval:= TMenuItem(Sender).Tag * 1000;
  TimerInterval.Interval:=Finterval;
end;

procedure TfrmFullscreen.ChangeMinFileSizeClicked(Sender: TObject);
var
  i:integer;
begin
  FMinimulFileSizeKiloByte := TMenuItem(Sender).Tag;

  TimerFadeIn.Enabled:=false;
  TimerFadeOut.Enabled:=false;
  TimerInterval.Enabled:=false;

  // Rebuild filelist index.

  FFilelist.Clear;
  //FFilelist.Assign(frmMain.FileList);
  // Loop & delete
  for i:=0 to frmMain.FileList.Count-1 do
  begin
    if FileSize(frmMain.FileList[i]) >= (FMinimulFileSizeKiloByte * 1024) then
    begin
      FFilelist.Add(frmMain.FileList[i]);
    end else
    begin
      // remove it from history and remainings.
      if FFileListRemaining.IndexOf(frmMain.FileList[i]) > -1 then
      begin
        FFileListRemaining.Delete(FFileListRemaining.IndexOf(frmMain.FileList[i]));
      end;
      if FFileListHistory.IndexOf(frmMain.FileList[i]) > -1 then
      begin
        FFileListHistory.Delete(FFileListHistory.IndexOf(frmMain.FileList[i]));
      end;
    end;
  end;

  if FFilelist.Count <= 0 then
  begin
    // Display error
    self.AlphaBlendValue:=255;
    Image1.Picture.Clear;
    with image1.Canvas do
      begin
        Brush.Style := bsClear;
        Font.Color := clWhite;
        TextOut(24,24, 'File size filter: File size too small.');
    end;
    Image1.Update;
    TimerInterval.Enabled:=false;
    TimerFadeOut.Enabled:=false;
    TimerFadeIn.Enabled:=false;
  end else
  begin

    // Set next index correctly.
    if FRandom then begin
      FiCurr:=-1;
      i:=GetNextRandomImageIndex();
      if (i > -1) then
      begin
        //FiCurr:=i;
        TimerInterval.tag:=i;
      end else
      begin
        FFileListRemaining.Assign(FFileList);
        FiCurr:=-1;
        i:=GetNextRandomImageIndex();
        TimerInterval.tag:=i;
      end;
    end else
    begin
      FiCurr:=0;
      TimerInterval.tag:=0;
    end;

    // Start
    if FManualTransition then
    begin
      // Reload previously showing image.  note:old index "FstrCurr" has been rebuilt.
      i:= FFilelist.IndexOf(FstrCurr);
      if (i > -1) and (FstrCurr <> '') then
      begin
        FiCurr:=-1;
        TimerInterval.tag:=i;

        PlaybackPlay(false);
      end;
    end else
    begin

      if FEffect then
      begin
        TimerFadeOut.Enabled:=true;
      end else
      begin
        //TimerInterval.Enabled:=true;
        TimerIntervalTimer(nil);
      end;

    end;
  end;


end;

procedure TfrmFullscreen.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_F11) or (Key = VK_ESCAPE) or (Chr(Key) = 'F')) then
  begin
    //close;
  end;


  if ((Key = VK_RMENU) or (Key = VK_LMENU) or (Key = VK_MENU)) then
  begin
    //self.PopupMenu1.PopUp(0,0);
  end;


  // Assigned in popupmenu shortcuts
  {
  if FManualTransition then begin

    //next
    if (Key = VK_RIGHT) then begin
      PlaybackNext(sender);
    end;
    //back
    if (Key = VK_LEFT) or (Key = VK_BACK) then begin
      PlaybackBack(sender);
    end;
    //pause/start
    if (Key = VK_PAUSE) or (Key = VK_SPACE) then
    begin
      PlaybackPause(sender);
    end;

  end;
  }
end;


procedure TfrmFullscreen.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Screen.Cursor:= crDefault;
  // Do the same at Image1.
end;

procedure TfrmFullscreen.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin

  PlaybackNext();
  //Handled:=true;

end;

procedure TfrmFullscreen.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin

  PlaybackBack();
  //Handled:=true;

end;

procedure TfrmFullscreen.IdleTimerMouseHideTimer(Sender: TObject);
begin
  if not FisPopupMenuShowing then
  begin
    Screen.Cursor:= crNone;
  end;
end;

procedure TfrmFullscreen.FormResize(Sender: TObject);
begin
  if FisInFrame then
  begin
    self.BoundsRect := self.Parent.ClientRect;
  end;
end;

procedure TfrmFullscreen.ShowFullScreen(blnOn: boolean);
begin
  FisFullscreen:=blnOn;
  {$ifdef windows}
  SetFullScreen_Win32(blnOn);
  {$else}
  SetFullScreen_Universal(blnOn);
  {$endif}
  //FisFullscreen:=blnOn;

  // No effect?
  if blnOn then
  begin
    //self.BringToFront;
    //SetForegroundWindow(self.Handle);
  end;
end;

procedure TfrmFullscreen.SetFullScreen_Universal(blnOn: boolean);
begin
  if blnOn then
  begin
    FOrigWndState:= WindowState;
    FOrigBounds := BoundsRect;
    {$ifdef windows}
    // don't do this at runtime on linux. It won't work anyway.
    BorderStyle:= bsNone;
    {$endif}
    // don't use this for modal window. And it won't work with multi moniters.
    //WindowState:=wsFullScreen;
    if (frmMain.CurrentMonitor <> Screen.Monitors[FOptIntMoniter]) then
    begin
      //TODO: need to test this.
      BoundsRect:= Screen.Monitors[FOptIntMoniter].BoundsRect;
      //BoundsRect:= frmMain.CurrentMonitor.BoundsRect;
    end else
    begin
      BoundsRect:= frmMain.CurrentMonitor.BoundsRect;
    end;
    ShowWindow(Handle, SW_SHOWFULLSCREEN)
  end else
  begin
    // we don't do this, because of the flickering when closing fullscreen window.
    //WindowState:= FOrigWndState;

    {$ifdef windows}
    //don't do this at runtime on linux! It won't work anyway.
    BorderStyle:= bsSizeable;
    {$endif}
    // On macOS, we have to call SW_SHOWNORMAL otherwise you don't get fullscreen next time.
    ShowWindow(Handle, SW_SHOWNORMAL);

    // we don't do this, because of the flickering when closing fullscreen window.
    //BoundsRect:= FOrigBounds;
  end;

end;

procedure TfrmFullscreen.SetFullScreen_Win32(blnOn: boolean);
begin
  if blnOn then
  begin
    // we don't need this because frmFullscreen is always fullscreen.
    {
    FOrigWndState:= WindowState;
    FOrigBounds:= BoundsRect;
    }
    BorderStyle:= bsNone;

    // not good when changing moniter at fullscreen form.
    //WindowState:=wsFullScreen;

    if (frmMain.CurrentMonitor <> Screen.Monitors[FOptIntMoniter]) then
    begin
      BoundsRect:= Screen.Monitors[FOptIntMoniter].BoundsRect;
      // need this when changin the moniter.
      ResizeImage();
    end else
    begin
      BoundsRect:= frmMain.CurrentMonitor.BoundsRect;
    end;

    //ShowWindow(Handle, SW_SHOWFULLSCREEN); //not good when changing moniter.
  end else
  begin
    // we don't do this, because of the flickering when closing fullscreen window.
    {
    WindowState:= FOrigWndState;
    BoundsRect:= FOrigBounds;
    BorderStyle:= bsSizeable;
    //ShowWindow(Handle, SW_SHOWNORMAL);
    BoundsRect:= FOrigBounds; //again
    }
  end;
end;



end.

