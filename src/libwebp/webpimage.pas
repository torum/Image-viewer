unit webpimage;

// libwebp (libwebp.dll wrapper. Implementation of reader, writter and TWEBPImage class)
// copyright @LacaK
// https://lacak.users.sourceforge.net/freepascal.html#libwebp

(******************************************************************************
 *                              TWEBPImage                                    *
 ******************************************************************************)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, FPImage, Graphics, IntfGraphics;

type

  { TSharedWEBPImage }

  TSharedWEBPImage = class(TSharedCustomBitmap)
  end;

  { TWEBPImage }

  TWEBPImage = class(TFPImageBitmap)  // = class(graphics.TCustomBitmap) = class(graphics.TRasterImage)
    private
      FQuality: integer; // The value 0 corresponds to low quality and small output sizes, whereas 100 is the highest quality and largest output size
    protected
      procedure InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader); override;
      procedure FinalizeReader(AReader: TFPCustomImageReader); override;
      procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
      class function GetReaderClass: TFPCustomImageReaderClass; override;
      class function GetWriterClass: TFPCustomImageWriterClass; override;
      class function GetSharedImageClass: TSharedRasterImageClass; override;
    public
      constructor Create; override;
      class function IsStreamFormatSupported(Stream: TStream): Boolean; override;
      class function GetFileExtensions: string; override;
    public
      property CompressionQuality: integer read FQuality write FQuality;
  end;

implementation

uses
  fpreadwebp;

constructor TWEBPImage.Create;
begin
  inherited;
  FQuality := 100;
end;

class function TWEBPImage.IsStreamFormatSupported(Stream: TStream): Boolean;
var
  Pos: Int64;
  Signature: array [0..3] of AnsiChar;
begin
  Pos := Stream.Position;
  try
    Stream.Position := 0;
    Result := Stream.Read(Signature, SizeOf(Signature))=4;
    Result := Result and (Signature='RIFF');
    Stream.Position := 8;
    Result := Result and (Stream.Read(Signature, SizeOf(Signature))=4);
    Result := Result and (Signature='WEBP');
  finally
    Stream.Position := Pos;
  end;
end;

procedure TWEBPImage.InitializeReader(AImage: TLazIntfImage;  AReader: TFPCustomImageReader);
begin
  // called from TFPImageBitmap.ReadStream:
  //   IntfImg := TLazIntfImage.Create(0,0,[]);
  //   InitializeReader(IntfImg, ImgReader);
  //   ImgReader.ImageRead(AStream, IntfImg); -> InternalCheck -> InternalRead
  //   FinalizeReader(ImgReader);
  inherited;
end;
procedure TWEBPImage.FinalizeReader(AReader: TFPCustomImageReader);
begin
  inherited;
end;

procedure TWEBPImage.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
begin
  // called when the WebP image is to be saved in another format
  inherited;
  if not(AWriter is TFPWriterWEBP) then Exit;
  TFPWriterWEBP(AWriter).CompressionQuality := CompressionQuality;
end;

class function TWEBPImage.GetFileExtensions: string;
begin
  Result := 'webp';
end;

class function TWEBPImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderWEBP;
end;

class function TWEBPImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterWEBP;
end;

class function TWEBPImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedWEBPImage;
end;


initialization
  TPicture.RegisterFileFormat('webp', 'WebP Image', TWEBPImage);

end.

