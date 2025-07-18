unit fpreadwebp;

// libwebp (libwebp.dll wrapper. Implementation of reader, writter and TWEBPImage class)
// copyright @LacaK
// https://lacak.users.sourceforge.net/freepascal.html#libwebp

// reads WEBP images. Requires "libwebp" library.
// writes WEBP images. Requires "libwebp" library.


{$mode ObjFPC}{$H+}

{$DEFINE INCREMENTAL_DECODING}

interface

uses
  Classes, SysUtils, FPImage;

type

  { TFPReaderWEBP }

  TFPReaderWEBP = class(TFPCustomImageReader)
    protected
      procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
      function  InternalCheck(Str: TStream): boolean; override;
  end;

  { TFPWriterWEBP }

  TFPWriterWEBP = class(TFPCustomImageWriter)
    private
      FQuality: integer;
    protected
      procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
    public
      constructor Create; override;
      property CompressionQuality: integer read FQuality write FQuality;
  end;

implementation

uses
  libwebp;

{ TFPReaderWEBP }

// ImageRead -> InternalCheck -> InternalRead
procedure TFPReaderWEBP.InternalRead(Str: TStream; Img: TFPCustomImage);
  function BGRAToFPColor(B,G,R,A: Byte): TFPColor; inline;
  begin
    with Result do
      begin
      Red   :=(R shl 8) or R;
      Green :=(G shl 8) or G;
      Blue  :=(B shl 8) or B;
      Alpha :=(A shl 8) or A;
      end;
  end;
var
  w,h,x,y: integer;
  ptr, p: Puint8_t;
  ContProgress: boolean;
{$IFDEF INCREMENTAL_DECODING}
  output: WebPDecBuffer;
  decoder: PWebPIDecoder;
  status: VP8StatusCode;
  buffer: array[0..4095] of Byte;
  data_size: integer;
{$ELSE}
  MStream: TMemoryStream;
{$ENDIF}
begin
  ContProgress:=True;
  Progress(psStarting, 0, False, Rect(0,0,0,0), '', ContProgress);
  if not ContProgress then Exit;

{$IFDEF INCREMENTAL_DECODING}
  FillByte(buffer, SizeOf(buffer), 0);
  data_size := Str.Read(buffer, SizeOf(buffer));
  if (data_size>0) and (WebPGetInfo(@buffer, SizeOf(buffer), @w, @h) = 0) then
    raise FPImageException.Create('Wrong WEBP image format!')
  else begin
    Img.SetSize(w, h);
    ptr := GetMem(4*w*h);

    WebPInitDecBuffer(@output);
    output.colorspace := MODE_BGRA;
    output.u.RGBA.rgba := ptr; // points to an external buffer
    output.u.RGBA.stride := 4*w;
    output.u.RGBA.size := 4*w*h;
    output.is_external_memory := 1;

    decoder := WebPINewDecoder(@output);
    if decoder <> nil then
      while data_size > 0 do begin
        status := WebPIAppend(decoder, @buffer[0], data_size);
        if (status <> VP8_STATUS_OK) and (status <> VP8_STATUS_SUSPENDED) then
          break;
        data_size := Str.Read(buffer, SizeOf(buffer));
      end;

    WebPFreeDecBuffer(@output);
    WebPIDelete(decoder); // config.output memory is preserved.
  end;
{$ELSE}
  MStream := TMemoryStream.Create;
  try
    MStream.LoadFromStream(Str);

    ptr := WebPDecodeBGRA(MStream.Memory, MStream.Size, @w, @h);
    if ptr = nil then
      raise FPImageException.Create('Wrong WEBP image format!')
    else begin
      Img.SetSize(w,h);

      p := ptr;
      for y:=0 to h-1 do begin
        for x:=0 to w-1 do
          // TFPCustomImage provides only setter method? (Does not implements memory bitmap where BGRA values can be written directly)
          Img.Colors[x,y] := BGRAToFPColor(p[x shl 2], p[(x shl 2)+1], p[(x shl 2)+2], p[(x shl 2)+3]);
        Inc(p, w shl 2);
      end;
    end;
  finally
    MStream.Free;
  end;
{$ENDIF}

  // WEBP image is fully decoded into buffer pointed by "ptr"
  p := ptr;
  for y:=0 to h-1 do begin
    for x:=0 to w-1 do
      // TFPCustomImage provides only setter method? (Does not implements memory bitmap where BGRA values can be written directly)
      Img.Colors[x,y] := BGRAToFPColor(p[x shl 2], p[(x shl 2)+1], p[(x shl 2)+2], p[(x shl 2)+3]);
    Inc(p, w shl 2);
  end;

{$IFDEF INCREMENTAL_DECODING}
  FreeMem(ptr);
{$ELSE}
  WebPFree(ptr);
{$ENDIF}
(*
  if WebPGetInfo(FStream.Memory, FStream.Size, @w, @h) <> 0 then begin
    Img.SetSize(w,h);
    bitmap := TBitmap.Create;
    bitmap.SetSize(w,h);
    bitmap.PixelFormat:=pf32bit; // BGRA
    if WebPDecodeBGRAInto(FStream.Memory, FStream.Size, bitmap.RawImage.Data, bitmap.RawImage.DataSize, bitmap.RawImage.Description.BytesPerLine) <> nil then
      Img.Assign(bitmap);
    bitmap.Free;
  end;
*)
  Progress(FPimage.psEnding, 100, False, Rect(0,0,w,h), '', ContProgress);
end;

function TFPReaderWEBP.InternalCheck(Str: TStream): boolean;
var
  p: Int64;
  Buf: array[0..3] of AnsiChar;
begin
  if Str=nil then Exit(False);
  p:=Str.Position;
  Str.Position:=0;
  Result := (Str.Read(Buf, 4)=4) and (Buf='RIFF');
  Str.Position:=8;
  Result := Result and (Str.Read(Buf, 4)=4) and (Buf='WEBP');
  Str.Position:=p;
end;


{ TFPWriterWEBP }

constructor TFPWriterWEBP.Create;
begin
  inherited;
  FQuality := 100;
end;

// TFPCustomImage.SaveToStream -> TFPCustomImageWriter.ImageWrite -> InternalWrite
procedure TFPWriterWEBP.InternalWrite(Str: TStream; Img: TFPCustomImage);
  function FPColorToBGRA(c: TFPColor): LongWord; inline;
  begin
    Result := (c.Blue shr 8) or ((c.Green shr 8) shl 8) or ((c.Red shr 8) shl 16) or ((c.Alpha shr 8) shl 24);
  end;
var
  bgra,p: PLongWord;
  x,y,data_size: integer;
  output: Puint8_t;
begin
  // first construct BGRA buffer from Img.Colors (is there smarter approach?)
  // (TFPCustomImage provides only getter/setter for Colors property)
  bgra := GetMem(Img.Width*Img.Height*4);
  p := bgra;
  for y:=0 to Img.Height-1 do begin
    for x:=0 to Img.Width-1 do
      p[x] := FPColorToBGRA(Img.Colors[x,y]);
    Inc(p, Img.Width);
  end;
  // then encode WEBP image
  data_size := WebPEncodeBGRA(Puint8_t(bgra), Img.Width, Img.Height, Img.Width*4, FQuality, output);
  // and save to stream
  Str.Write(output^, data_size);
  WebPFree(output);
  FreeMem(bgra);
end;


initialization
  if LoadLibwebp() then begin
    ImageHandlers.RegisterImageReader('WEBP Graphics', 'webp', TFPReaderWEBP);
    ImageHandlers.RegisterImageWriter('WEBP Graphics', 'webp', TFPWriterWEBP);
  end;

end.

