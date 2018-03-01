unit UAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, lclintf;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Bevel1: TBevel;
    ButtonClose: TButton;
    Image1: TImage;
    LabelWebsite: TLabel;
    StaticTextAppsVer: TStaticText;
    StaticTextWho: TStaticText;
    StaticTextWebSite: TStaticText;
    procedure StaticTextWebSiteClick(Sender: TObject);
    procedure StaticTextWebSiteMouseEnter(Sender: TObject);
    procedure StaticTextWebSiteMouseLeave(Sender: TObject);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.StaticTextWebSiteClick(Sender: TObject);
begin
  OpenURL(StaticTextWebSite.Caption);
end;

procedure TfrmAbout.StaticTextWebSiteMouseEnter(Sender: TObject);
begin
  StaticTextWebSite.Cursor := crHandPoint;
  StaticTextWebSite.Font.Color := clBlue;
  StaticTextWebSite.Font.Style:=StaticTextWebSite.Font.Style+[fsUnderline];
end;

procedure TfrmAbout.StaticTextWebSiteMouseLeave(Sender: TObject);
begin
  StaticTextWebSite.Font.Color := clDefault;
  StaticTextWebSite.Font.Style:=StaticTextWebSite.Font.Style-[fsUnderline];
end;

end.

