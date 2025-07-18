unit libwebp;
// Copyright 2010 Google Inc. All Rights Reserved.
//
// Use of this source code is governed by a BSD-style license
// that can be found in the COPYING file in the root of the source
// tree. An additional intellectual property rights grant can be found
// in the file PATENTS. All contributing project authors may
// be found in the AUTHORS file in the root of the source tree.
// -----------------------------------------------------------------------------
//

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4} // GCC on x86 enums have size of 4 bytes
  {$PACKRECORDS C}
{$ELSE}
  {$MINENUMSIZE 4}
  {$ALIGN 4}
{$ENDIF}
{$H+}
{$POINTERMATH ON}

interface

const
  {$IFDEF UNIX}
    libwebp = 'libwebp.so';
  {$ELSE}
    libwebp = 'libwebp_static.dll';    //         'libwebp.dll'
  {$ENDIF}

(**********************************
 * Common types + memory wrappers *
 **********************************)
type
  int = integer;
  uint8_t = Byte;
  uint32_t = LongWord;
  float = single;
  Pint = ^int;
  Puint8_t = ^uint8_t;
  Puint32_t = ^uint32_t;
  {$IF NOT DECLARED(size_t)}
  size_t = NativeUInt;
  {$IFEND}

(*******************************************
 * Main decoding functions for WebP images *
 *******************************************)
const
  WEBP_DECODER_ABI_VERSION = $0209; // MAJOR(8b) + MINOR(8b)
  WEBP_ENCODER_ABI_VERSION = $020f; // MAJOR(8b) + MINOR(8b)

type
  // Enumeration of the status codes
  VP8StatusCode = (
    VP8_STATUS_OK = 0,
    VP8_STATUS_OUT_OF_MEMORY,
    VP8_STATUS_INVALID_PARAM,
    VP8_STATUS_BITSTREAM_ERROR,
    VP8_STATUS_UNSUPPORTED_FEATURE,
    VP8_STATUS_SUSPENDED,
    VP8_STATUS_USER_ABORT,
    VP8_STATUS_NOT_ENOUGH_DATA
  );

  WEBP_CSP_MODE = (
    MODE_RGB = 0, MODE_RGBA = 1,
    MODE_BGR = 2, MODE_BGRA = 3,
    MODE_ARGB = 4, MODE_RGBA_4444 = 5,
    MODE_RGB_565 = 6,
    // RGB-premultiplied transparent modes (alpha value is preserved)
    MODE_rgbA_ = 7,
    MODE_bgrA_ = 8,
    MODE_Argb_ = 9,
    MODE_rgbA_4444_ = 10,
    // YUV modes must come after RGB ones.
    MODE_YUV = 11, MODE_YUVA = 12,  // yuv 4:2:0
    MODE_LAST = 13
  );

  // WebPDecBuffer: Generic structure for describing the output sample buffer.

  WebPRGBABuffer = record       // view as RGBA
    rgba: Puint8_t;             // pointer to RGBA samples
    stride: int;                // stride in bytes from one scanline to the next.
    size: size_t;               // total size of the *rgba buffer.
  end;

  WebPYUVABuffer = record       // view as YUVA
    y, u, v, a: Puint8_t;       // pointer to luma, chroma U/V, alpha samples
    y_stride: int;              // luma stride
    u_stride, v_stride: int;    // chroma strides
    a_stride: int;              // alpha stride
    y_size: size_t;             // luma plane size
    u_size, v_size: size_t;     // chroma planes size
    a_size: size_t;             // alpha-plane size
  end;

  // Output buffer
  WebPDecBuffer = record
    colorspace: WEBP_CSP_MODE;  // Colorspace.
    width, height: int;         // Dimensions.
    is_external_memory: int;    // If non-zero, 'internal_memory' pointer is not
                                // used. If value is '2' or more, the external
                                // memory is considered 'slow' and multiple
                                // read/write will be avoided.
    u: record {union}
       case longint of
         0: (RGBA: WebPRGBABuffer);
         1: (YUVA: WebPYUVABuffer);
       end;

    pad: array[0..3] of uint32_t;     // padding for later use

    private_memory: Puint8_t;   // Internally allocated memory (only when
                                // is_external_memory is 0). Should not be used
                                // externally, but accessed via the buffer union.
  end;
  PWebPDecBuffer = ^WebPDecBuffer;

  // Features gathered from the bitstream
  WebPBitstreamFeatures = record
    width: int;          // Width in pixels, as read from the bitstream.
    height: int;         // Height in pixels, as read from the bitstream.
    has_alpha: int;      // True if the bitstream contains an alpha channel.
    has_animation: int;  // True if the bitstream is an animation.
    format: int;         // 0 = undefined (/mixed), 1 = lossy, 2 = lossless

    pad: array[0..4] of uint32_t;    // padding for later use
  end;
  PWebPBitstreamFeatures = ^WebPBitstreamFeatures;

  // Decoding options
  WebPDecoderOptions = record
    bypass_filtering: int;               // if true, skip the in-loop filtering
    no_fancy_upsampling: int;            // if true, use faster pointwise upsampler
    use_cropping: int;                   // if true, cropping is applied _first_
    crop_left, crop_top: int;            // top-left position for cropping.
                                         // Will be snapped to even values.
    crop_width, crop_height: int;        // dimension of the cropping area
    use_scaling: int;                    // if true, scaling is applied _afterward_
    scaled_width, scaled_height: int;    // final resolution
    use_threads: int;                    // if true, use multi-threaded decoding
    dithering_strength: int;             // dithering strength (0=Off, 100=full)
    flip: int;                           // if true, flip output vertically
    alpha_dithering_strength: int;       // alpha dithering strength in [0..100]

    pad: array[0..4] of uint32_t;        // padding for later use
  end;

  // Main object storing the configuration for advanced decoding.
  WebPDecoderConfig = record
    input: WebPBitstreamFeatures;  // Immutable bitstream features (optional)
    output: WebPDecBuffer;         // Output buffer (can point to external mem)
    options: WebPDecoderOptions;   // Decoding options
  end;
  PWebPDecoderConfig = ^WebPDecoderConfig;

  WebPIDecoder = record end;
  PWebPIDecoder = ^WebPIDecoder;

  //-------------------
  // Coding parameters
  //-------------------
  // Image characteristics hint for the underlying encoder.
  WebPImageHint = (
    WEBP_HINT_DEFAULT = 0,  // default preset.
    WEBP_HINT_PICTURE,      // digital picture, like portrait, inner shot
    WEBP_HINT_PHOTO,        // outdoor photograph, with natural lighting
    WEBP_HINT_GRAPH,        // Discrete tone image (graph, map-tile etc).
    WEBP_HINT_LAST
  );

  // Compression parameters.
  WebPConfig = record
    lossless: int;          // Lossless encoding (0=lossy(default), 1=lossless).
    quality: float;         // between 0 and 100. For lossy, 0 gives the smallest
                            // size and 100 the largest. For lossless, this
                            // parameter is the amount of effort put into the
                            // compression: 0 is the fastest but gives larger
                            // files compared to the slowest, but best, 100.
    method: int;            // quality/speed trade-off (0=fast, 6=slower-better)

    image_hint:WebPImageHint;  // Hint for image type (lossless only for now).

    target_size: int;       // if non-zero, set the desired target size in bytes.
                            // Takes precedence over the 'compression' parameter.
    target_PSNR: float;     // if non-zero, specifies the minimal distortion to
                            // try to achieve. Takes precedence over target_size.
    segments: int;          // maximum number of segments to use, in [1..4]
    sns_strength: int;      // Spatial Noise Shaping. 0=off, 100=maximum.
    filter_strength: int;   // range: [0 = off .. 100 = strongest]
    filter_sharpness: int;  // range: [0 = off .. 7 = least sharp]
    filter_type: int;       // filtering type: 0 = simple, 1 = strong (only used
                            // if filter_strength > 0 or autofilter > 0)
    autofilter: int;        // Auto adjust filter's strength [0 = off, 1 = on]
    alpha_compression: int; // Algorithm for encoding the alpha plane (0 = none,
                            // 1 = compressed with WebP lossless). Default is 1.
    alpha_filtering: int;   // Predictive filtering method for alpha plane.
                            //  0: none, 1: fast, 2: best. Default if 1.
    alpha_quality: int;     // Between 0 (smallest size) and 100 (lossless).
                            // Default is 100.
    pass: int;              // number of entropy-analysis passes (in [1..10]).

    show_compressed: int;   // if true, export the compressed picture back.
                            // In-loop filtering is not applied.
    preprocessing: int;     // preprocessing filter:
                            // 0=none, 1=segment-smooth, 2=pseudo-random dithering
    partitions: int;        // log2(number of token partitions) in [0..3]. Default
                            // is set to 0 for easier progressive decoding.
    partition_limit: int;   // quality degradation allowed to fit the 512k limit
                            // on prediction modes coding (0: no degradation,
                            // 100: maximum possible degradation).
    emulate_jpeg_size: int; // If true, compression parameters will be remapped
                            // to better match the expected output size from
                            // JPEG compression. Generally, the output size will
                            // be similar but the degradation will be lower.
    thread_level: int;      // If non-zero, try and use multi-threaded encoding.
    low_memory: int;        // If set, reduce memory usage (but increase CPU use).

    near_lossless: int;     // Near lossless encoding [0 = max loss .. 100 = off
                            // (default)].
    exact: int;             // if non-zero, preserve the exact RGB values under
                            // transparent area. Otherwise, discard this invisible
                            // RGB information for better compression. The default
                            // value is 0.

    use_delta_palette: int; // reserved for future lossless feature
    use_sharp_yuv: int;     // if needed, use sharp (and slow) RGB->YUV conversion

    qmin: int;              // minimum permissible quality factor
    qmax: int;              // maximum permissible quality factor
  end;
  PWebPConfig = ^WebPConfig;

  // Enumerate some predefined settings for WebPConfig, depending on the type
  // of source picture. These presets are used when calling WebPConfigPreset().
  WebPPreset = (
    WEBP_PRESET_DEFAULT = 0,  // default preset.
    WEBP_PRESET_PICTURE,      // digital picture, like portrait, inner shot
    WEBP_PRESET_PHOTO,        // outdoor photograph, with natural lighting
    WEBP_PRESET_DRAWING,      // hand or line drawing, with high-contrast details
    WEBP_PRESET_ICON,         // small-sized colorful images
    WEBP_PRESET_TEXT          // text-like
  );

  // Structure for storing auxiliary statistics.
  WebPAuxStats = record
    coded_size: int;        // final size

    PSNR: array[0..4] of float; // peak-signal-to-noise ratio for Y/U/V/All/Alpha
    block_count: array[0..2] of int;     // number of intra4/intra16/skipped macroblocks
    header_bytes: array[0..1] of int;    // approximate number of bytes spent for header
                            // and mode-partition #0
    residual_bytes: array[0..2,0..3] of int;  // approximate number of bytes spent for
                               // DC/AC/uv coefficients for each (0..3) segments.
    segment_size: array[0..3] of int;    // number of macroblocks in each segments
    segment_quant: array[0..3] of int;   // quantizer values for each segments
    segment_level: array[0..3] of int;   // filtering strength for each segments [0..63]

    alpha_data_size: int;    // size of the transparency data
    layer_data_size: int;    // size of the enhancement layer data

    // lossless encoder statistics
    lossless_features: uint32_t; // bit0:predictor bit1:cross-color transform
                                 // bit2:subtract-green bit3:color indexing
    histogram_bits: int;         // number of precision bits of histogram
    transform_bits: int;         // precision bits for transform
    cache_bits: int;             // number of bits for color cache lookup
    palette_size: int;           // number of color in palette, if used
    lossless_size: int;          // final lossless size
    lossless_hdr_size: int;      // lossless header (transform, huffman etc) size
    lossless_data_size: int;     // lossless image data size

    pad: array[0..1] of uint32_t;  // padding for later use
  end;
  PWebPAuxStats = ^WebPAuxStats;

  // WebPMemoryWrite: a special WebPWriterFunction that writes to memory using
  // the following WebPMemoryWriter object (to be set as a custom_ptr).
  WebPMemoryWriter = record
    mem: Puint8_t;      // final buffer (of size 'max_size', larger than 'size').
    size: size_t;       // final size
    max_size: size_t;   // total capacity
    pad: array[0..0] of uint32_t;    // padding for later use
  end;
  PWebPMemoryWriter = ^WebPMemoryWriter;

  // Color spaces.
  WebPEncCSP = (
    // chroma sampling
    WEBP_YUV420  = 0,        // 4:2:0
    WEBP_YUV420A = 4,        // alpha channel variant
    WEBP_CSP_UV_MASK = 3,    // bit-mask to get the UV sampling factors
    WEBP_CSP_ALPHA_BIT = 4   // bit that is set if alpha is present
  );

  // Encoding error conditions.
  WebPEncodingError = (
    VP8_ENC_OK = 0,
    VP8_ENC_ERROR_OUT_OF_MEMORY,            // memory error allocating objects
    VP8_ENC_ERROR_BITSTREAM_OUT_OF_MEMORY,  // memory error while flushing bits
    VP8_ENC_ERROR_NULL_PARAMETER,           // a pointer parameter is NULL
    VP8_ENC_ERROR_INVALID_CONFIGURATION,    // configuration is invalid
    VP8_ENC_ERROR_BAD_DIMENSION,            // picture has invalid width/height
    VP8_ENC_ERROR_PARTITION0_OVERFLOW,      // partition is bigger than 512k
    VP8_ENC_ERROR_PARTITION_OVERFLOW,       // partition is bigger than 16M
    VP8_ENC_ERROR_BAD_WRITE,                // error while flushing bytes
    VP8_ENC_ERROR_FILE_TOO_BIG,             // file is bigger than 4G
    VP8_ENC_ERROR_USER_ABORT,               // abort request by user
    VP8_ENC_ERROR_LAST                      // list terminator. always last.
  );

  // Signature for output function. Should return true if writing was successful.
  // data/data_size is the segment of data to write, and 'picture' is for
  // reference (and so one can make use of picture->custom_ptr).
  PWebPPicture = ^WebPPicture;
  WebPWriterFunction = function(const data: Puint8_t; data_size: size_t; const picture: PWebPPicture): int; cdecl;

  // Progress hook, called from time to time to report progress. It can return
  // false to request an abort of the encoding process, or true otherwise if
  // everything is OK.
  WebPProgressHook = function(percent: int; const picture: PWebPPicture): int; cdecl;

  // Main exchange structure (input samples, output bytes, statistics)
  //
  // Once WebPPictureInit() has been called, it's ok to make all the INPUT fields
  // (use_argb, y/u/v, argb, ...) point to user-owned data, even if
  // WebPPictureAlloc() has been called. Depending on the value use_argb,
  // it's guaranteed that either *argb or *y/*u/*v content will be kept untouched.
  WebPPicture = record
    //   INPUT
    //////////////
    // Main flag for encoder selecting between ARGB or YUV input.
    // It is recommended to use ARGB input (*argb, argb_stride) for lossless
    // compression, and YUV input (*y, *u, *v, etc.) for lossy compression
    // since these are the respective native colorspace for these formats.
    use_argb: int;

    // YUV input (mostly used for input to lossy compression)
    colorspace: WebPEncCSP;    // colorspace: should be YUV420 for now (=Y'CbCr).
    width, height: int;        // dimensions (less or equal to WEBP_MAX_DIMENSION)
    y, u, v: Puint8_t ;        // pointers to luma/chroma planes.
    y_stride, uv_stride: int;  // luma/chroma strides.
    a: Puint8_t;               // pointer to the alpha plane
    a_stride: int;             // stride of the alpha plane
    pad1: array[0..1] of uint32_t;          // padding for later use

    // ARGB input (mostly used for input to lossless compression)
    argb: Puint32_t;           // Pointer to argb (32 bit) plane.
    argb_stride: int;          // This is stride in pixels units, not bytes.
    pad2: array[0..2] of uint32_t;          // padding for later use

    //   OUTPUT
    ///////////////
    // Byte-emission hook, to store compressed bytes as they are ready.
    writer: WebPWriterFunction; // can be NULL
    custom_ptr: pointer;        // can be used by the writer.

    // map for extra information (only for lossy compression mode)
    extra_info_type: int;   // 1: intra type, 2: segment, 3: quant
                            // 4: intra-16 prediction mode,
                            // 5: chroma prediction mode,
                            // 6: bit cost, 7: distortion
    extra_info: Puint8_t;   // if not NULL, points to an array of size
                            // ((width + 15) / 16) * ((height + 15) / 16) that
                            // will be filled with a macroblock map, depending
                            // on extra_info_type.

    //   STATS AND REPORTS
    ///////////////////////////
    // Pointer to side statistics (updated only if not NULL)
    stats: PWebPAuxStats;

    // Error code for the latest error encountered during encoding
    error_code: WebPEncodingError;

    // If not NULL, report progress during encoding.
    progress_hook: WebPProgressHook;

    user_data: pointer;     // this field is free to be set to any value and
                            // used during callbacks (like progress-report e.g.).

    pad3: array[0..2] of uint32_t;       // padding for later use

    // Unused for now
    pad4, pad5: Puint8_t;
    pad6: array[0..7] of uint32_t;       // padding for later use

    // PRIVATE FIELDS
    ////////////////////
    memory_: pointer;          // row chunk of memory for yuva planes
    memory_argb_: pointer;     // and for argb too.
    pad7: array[0..1] of pointer;        // padding for later use
  end;

var
  // Allocates 'size' bytes of memory. Returns NULL upon error. Memory
  // must be deallocated by calling WebPFree(). This function is made available
  // by the core 'libwebp' library.
  WebPMalloc: function(size: size_t): Pointer; cdecl;

  // Releases memory returned by the WebPDecode*() functions (from decode.h).
  WebPFree: procedure(ptr: Pointer); cdecl;

  (*******************************************
   * Main decoding functions for WebP images *
   *******************************************)

  // Return the decoder's version number, packed in hexadecimal using 8bits for
  // each of major/minor/revision. E.g: v2.5.7 is 0x020507.
  WebPGetDecoderVersion: function(): int; cdecl;

  // Retrieve basic header information: width, height.
  // This function will also validate the header, returning true on success,
  // false otherwise. '*width' and '*height' are only valid on successful return.
  // Pointers 'width' and 'height' can be passed NULL if deemed irrelevant.
  // Note: The following chunk sequences (before the raw VP8/VP8L data) are
  // considered valid by this function:
  // RIFF + VP8(L)
  // RIFF + VP8X + (optional chunks) + VP8(L)
  // ALPH + VP8 <-- Not a valid WebP format: only allowed for internal purpose.
  // VP8(L)     <-- Not a valid WebP format: only allowed for internal purpose.
  WebPGetInfo: function(const data: Puint8_t; data_size: size_t; width, height: Pint): int; cdecl;

  // Decodes WebP images pointed to by 'data' and returns RGBA samples, along
  // with the dimensions in *width and *height. The ordering of samples in
  // memory is R, G, B, A, R, G, B, A... in scan order (endian-independent).
  // The returned pointer should be deleted calling WebPFree().
  // Returns NULL in case of error.
  WebPDecodeRGBA: function (const data: Puint8_t; data_size: size_t; width, height: Pint): Puint8_t; cdecl;

  // Same as WebPDecodeRGBA, but returning A, R, G, B, A, R, G, B... ordered data.
  WebPDecodeARGB: function(const data: Puint8_t; data_size: size_t; width, height: Pint): Puint8_t; cdecl;

  // Same as WebPDecodeRGBA, but returning B, G, R, A, B, G, R, A... ordered data.
  WebPDecodeBGRA: function(const data: Puint8_t; data_size: size_t; width, height: Pint): Puint8_t; cdecl;

  // Same as WebPDecodeRGBA, but returning R, G, B, R, G, B... ordered data.
  // If the bitstream contains transparency, it is ignored.
  WebPDecodeRGB: function(const data: Puint8_t; data_size: size_t; width, height: Pint): Puint8_t; cdecl;

  // Same as WebPDecodeRGB, but returning B, G, R, B, G, R... ordered data.
  WebPDecodeBGR: function(const data: Puint8_t; data_size: size_t; width, height: Pint): Puint8_t; cdecl;

  // These five functions are variants of the above ones, that decode the image
  // directly into a pre-allocated buffer 'output_buffer'. The maximum storage
  // available in this buffer is indicated by 'output_buffer_size'. If this
  // storage is not sufficient (or an error occurred), NULL is returned.
  // Otherwise, output_buffer is returned, for convenience.
  // The parameter 'output_stride' specifies the distance (in bytes)
  // between scanlines. Hence, output_buffer_size is expected to be at least
  // output_stride x picture-height.
  WebPDecodeRGBAInto: function(const data: Puint8_t; data_size: size_t; output_buffer: Puint8_t; output_buffer_size: size_t; output_stride: int): Puint8_t; cdecl;
  WebPDecodeARGBInto: function(const data: Puint8_t; data_size: size_t; output_buffer: Puint8_t; output_buffer_size: size_t; output_stride: int): Puint8_t; cdecl;
  WebPDecodeBGRAInto: function(const data: Puint8_t; data_size: size_t; output_buffer: Puint8_t; output_buffer_size: size_t; output_stride: int): Puint8_t; cdecl;
  // RGB and BGR variants. Here too the transparency information, if present,
  // will be dropped and ignored.
  WebPDecodeRGBInto: function(const data: Puint8_t; data_size: size_t; output_buffer: Puint8_t; output_buffer_size: size_t; output_stride: int): Puint8_t; cdecl;
  WebPDecodeBGRInto: function(const data: Puint8_t; data_size: size_t; output_buffer: Puint8_t; output_buffer_size: size_t; output_stride: int): Puint8_t; cdecl;


  // Internal, version-checked, entry point
  WebPInitDecBufferInternal: function(buffer: PWebPDecBuffer; version: int): int; cdecl;

  // Free any memory associated with the buffer. Must always be called last.
  // Note: doesn't free the 'buffer' structure itself.
  WebPFreeDecBuffer: procedure(buffer: PWebPDecBuffer); cdecl;

  //----------------------
  // Incremental decoding
  //----------------------

  // Creates a new incremental decoder with the supplied buffer parameter.
  // This output_buffer can be passed NULL, in which case a default output buffer
  // is used (with MODE_RGB). Otherwise, an internal reference to 'output_buffer'
  // is kept, which means that the lifespan of 'output_buffer' must be larger than
  // that of the returned WebPIDecoder object.
  // The supplied 'output_buffer' content MUST NOT be changed between calls to
  // WebPIAppend() or WebPIUpdate() unless 'output_buffer.is_external_memory' is
  // not set to 0. In such a case, it is allowed to modify the pointers, size and
  // stride of output_buffer.u.RGBA or output_buffer.u.YUVA, provided they remain
  // within valid bounds.
  // All other fields of WebPDecBuffer MUST remain constant between calls.
  // Returns NULL if the allocation failed.
  WebPINewDecoder: function(output_buffer: PWebPDecBuffer): PWebPIDecoder; cdecl;

  // Deletes the WebPIDecoder object and associated memory. Must always be called
  // if WebPINewDecoder, WebPINewRGB or WebPINewYUV succeeded.
  WebPIDelete: procedure(idec: PWebPIDecoder); cdecl;

  // Copies and decodes the next available data. Returns VP8_STATUS_OK when
  // the image is successfully decoded. Returns VP8_STATUS_SUSPENDED when more
  // data is expected. Returns error in other cases.
  WebPIAppend: function(idec: PWebPIDecoder; const data: Puint8_t; data_size: size_t): VP8StatusCode; cdecl;

  // A variant of the above function to be used when data buffer contains
  // partial data from the beginning. In this case data buffer is not copied
  // to the internal memory.
  // Note that the value of the 'data' pointer can change between calls to
  // WebPIUpdate, for instance when the data buffer is resized to fit larger data.
  WebPIUpdate: function(idec: PWebPIDecoder; const data: Puint8_t; data_size: size_t): VP8StatusCode; cdecl;

  // Returns the RGB/A image decoded so far. Returns NULL if output params
  // are not initialized yet. The RGB/A output type corresponds to the colorspace
  // specified during call to WebPINewDecoder() or WebPINewRGB().
  // *last_y is the index of last decoded row in raster scan order. Some pointers
  // (*last_y, *width etc.) can be NULL if corresponding information is not
  // needed. The values in these pointers are only valid on successful (non-NULL)
  // return.
  WebPIDecGetRGB: function(const idec: PWebPIDecoder; last_y, width, height, stride: Pint): Puint8_t; cdecl;

  //-----------------------------------
  // Advanced decoding parametrization
  //-----------------------------------

  // Internal, version-checked, entry point
  WebPGetFeaturesInternal: function(const data: Puint8_t; data_size: size_t; features: PWebPBitstreamFeatures; version: int): VP8StatusCode; cdecl;
  // Internal, version-checked, entry point
  WebPInitDecoderConfigInternal: function(config: PWebPDecoderConfig; version: int): int; cdecl;

  // Instantiate a new incremental decoder object with the requested
  // configuration. The bitstream can be passed using 'data' and 'data_size'
  // parameter, in which case the features will be parsed and stored into
  // config->input. Otherwise, 'data' can be NULL and no parsing will occur.
  // Note that 'config' can be NULL too, in which case a default configuration
  // is used. If 'config' is not NULL, it must outlive the WebPIDecoder object
  // as some references to its fields will be used. No internal copy of 'config'
  // is made.
  // The return WebPIDecoder object must always be deleted calling WebPIDelete().
  // Returns NULL in case of error (and config->status will then reflect
  // the error condition, if available).
  WebPIDecode: function(const data: Puint8_t; data_size: size_t; config: PWebPDecoderConfig): PWebPIDecoder; cdecl;

  // Non-incremental version. This version decodes the full data at once, taking
  // 'config' into account. Returns decoding status (which should be VP8_STATUS_OK
  // if the decoding was successful). Note that 'config' cannot be NULL.
  WebPDecode: function(const data: Puint8_t; data_size: size_t; config: PWebPDecoderConfig): VP8StatusCode; cdecl;


  (********************************
   * WebP encoder: main interface *
   ********************************)

   // Return the encoder's version number, packed in hexadecimal using 8bits for
   // each of major/minor/revision. E.g: v2.5.7 is 0x020507.
   WebPGetEncoderVersion: function(): int; cdecl;

   // Returns the size of the compressed data (pointed to by *output), or 0 if
   // an error occurred. The compressed data must be released by the caller
   // using the call 'WebPFree(*output)'.
   // These functions compress using the lossy format, and the quality_factor
   // can go from 0 (smaller output, lower quality) to 100 (best quality,
   // larger output).
   WebPEncodeRGB: function(const rgb: Puint8_t; width, height, stride: int; quality_factor: float; var output: Puint8_t): size_t; cdecl;
   WebPEncodeBGR: function(const bgr: Puint8_t; width, height, stride: int; quality_factor: float; var output: Puint8_t): size_t; cdecl;
   WebPEncodeRGBA: function(const rgba: Puint8_t; width, height, stride: int; quality_factor: float; var output: Puint8_t): size_t; cdecl;
   WebPEncodeBGRA: function(const bgra: Puint8_t; width, height, stride: int; quality_factor: float; var output: Puint8_t): size_t; cdecl;

   // These functions are the equivalent of the above, but compressing in a
   // lossless manner. Files are usually larger than lossy format, but will
   // not suffer any compression loss.
   // Note these functions, like the lossy versions, use the library's default
   // settings. For lossless this means 'exact' is disabled. RGB values in
   // transparent areas will be modified to improve compression. To avoid this,
   // use WebPEncode() and set WebPConfig::exact to 1.
   WebPEncodeLosslessRGB: function(const rgb: Puint8_t; width, height, stride: int; var output: Puint8_t): size_t; cdecl;
   WebPEncodeLosslessBGR: function(const bgr: Puint8_t; width, height, stride: int; var output: Puint8_t): size_t; cdecl;
   WebPEncodeLosslessRGBA: function(const rgba: Puint8_t; width, height, stride: int; var output: Puint8_t): size_t; cdecl;
   WebPEncodeLosslessBGRA: function(const bgra: Puint8_t; width, height, stride: int; var output: Puint8_t): size_t; cdecl;

   // Internal, version-checked, entry point
   WebPConfigInitInternal: function(config: PWebPConfig; preset: WebPPreset; quality: float; version: int): int; cdecl;

   // Activate the lossless compression mode with the desired efficiency level
   // between 0 (fastest, lowest compression) and 9 (slower, best compression).
   // A good default level is '6', providing a fair tradeoff between compression
   // speed and final compressed size.
   // This function will overwrite several fields from config: 'method', 'quality'
   // and 'lossless'. Returns false in case of parameter error.
   WebPConfigLosslessPreset: function(config: PWebPConfig; level: int): int; cdecl;

   // Returns true if 'config' is non-NULL and all configuration parameters are
   // within their valid ranges.
   WebPValidateConfig: function(const config: PWebPConfig): int; cdecl;

   // The following must be called first before any use.
   WebPMemoryWriterInit: procedure(writer: PWebPMemoryWriter); cdecl;
   // The following must be called to deallocate writer->mem memory. The 'writer'
   // object itself is not deallocated.
   WebPMemoryWriterClear: procedure(writer: PWebPMemoryWriter); cdecl;
   // The custom writer to be used with WebPMemoryWriter as custom_ptr. Upon
   // completion, writer.mem and writer.size will hold the coded data.
   // writer.mem must be freed by calling WebPMemoryWriterClear.
   WebPMemoryWrite: WebPWriterFunction;

   // Internal, version-checked, entry point
   WebPPictureInitInternal: function(picture: PWebPPicture; version: int): int; cdecl;

   // Convenience allocation / deallocation based on picture->width/height:
   // Allocate y/u/v buffers as per colorspace/width/height specification.
   // Note! This function will free the previous buffer if needed.
   // Returns false in case of memory error.
   WebPPictureAlloc: function(picture: PWebPPicture): int; cdecl;

   // Release the memory allocated by WebPPictureAlloc() or WebPPictureImport*().
   // Note that this function does _not_ free the memory used by the 'picture'
   // object itself.
   // Besides memory (which is reclaimed) all other fields of 'picture' are
   // preserved.
   WebPPictureFree: procedure(picture: PWebPPicture); cdecl;

   // Main encoding call, after config and picture have been initialized.
   // 'picture' must be less than 16384x16384 in dimension (cf WEBP_MAX_DIMENSION),
   // and the 'config' object must be a valid one.
   // Returns false in case of error, true otherwise.
   // In case of error, picture->error_code is updated accordingly.
   // 'picture' can hold the source samples in both YUV(A) or ARGB input, depending
   // on the value of 'picture->use_argb'. It is highly recommended to use
   // the former for lossy encoding, and the latter for lossless encoding
   // (when config.lossless is true). Automatic conversion from one format to
   // another is provided but they both incur some loss.
   WebPEncode: function(const config: PWebPConfig; picture: PWebPPicture): int; cdecl;

function WebPGetFeatures(const data: Puint8_t; data_size: size_t; features: PWebPBitstreamFeatures): VP8StatusCode; inline;
function WebPInitDecBuffer(buffer: PWebPDecBuffer): int; inline;
function WebPInitDecoderConfig(config: PWebPDecoderConfig): int; inline;

function WebPConfigInit(config: PWebPConfig): int; inline;
function WebPConfigPreset(config: PWebPConfig; preset: WebPPreset; quality: float): int; inline;
function WebPPictureInit(picture: PWebPPicture): int; inline;

function LoadLibwebp(): boolean;
procedure UnloadLibwebp();

implementation

uses
  {$IFDEF MSWINDOWS}Windows{$ELSE}DynLibs{$ENDIF};

var
  LibwebpHandle: {$IFDEF MSWINDOWS}THandle{$ELSE}TLibHandle{$ENDIF};
  RefCount: integer;

{$IFDEF MSWINDOWS}
function GetProcedureAddress(Lib: THandle; ProcName: PAnsiChar): Pointer; inline;
begin
  Result := Windows.GetProcAddress(Lib, ProcName);
end;
function UnloadLibrary(Lib: THandle): Boolean; inline;
begin
  Result := Windows.FreeLibrary(Lib);
end;
{$ENDIF}

function LoadLibwebp(): boolean;
begin
  Inc(RefCount);
  if RefCount = 1 then begin
    LibwebpHandle := LoadLibrary(libwebp);//(libwebp);
    if LibwebpHandle = 0 then begin
      RefCount := 0;
      //raise EInOutError.CreateFmt('Can not load library "%s". Check your installation.', [libwebp]);
      Exit(False);
    end;

    @WebPMalloc := GetProcedureAddress(LibwebpHandle,'WebPMalloc');
    @WebPFree := GetProcedureAddress(LibwebpHandle,'WebPFree');

    @WebPGetDecoderVersion := GetProcedureAddress(LibwebpHandle,'WebPGetDecoderVersion');
    @WebPGetInfo := GetProcedureAddress(LibwebpHandle,'WebPGetInfo');

    @WebPDecodeRGBA := GetProcedureAddress(LibwebpHandle,'WebPDecodeRGBA');
    @WebPDecodeARGB := GetProcedureAddress(LibwebpHandle,'WebPDecodeARGB');
    @WebPDecodeBGRA := GetProcedureAddress(LibwebpHandle,'WebPDecodeBGRA');
    @WebPDecodeRGB := GetProcedureAddress(LibwebpHandle,'WebPDecodeRGB');
    @WebPDecodeBGR := GetProcedureAddress(LibwebpHandle,'WebPDecodeBGR');

    @WebPDecodeRGBAInto := GetProcedureAddress(LibwebpHandle,'WebPDecodeRGBAInto');
    @WebPDecodeARGBInto := GetProcedureAddress(LibwebpHandle,'WebPDecodeARGBInto');
    @WebPDecodeBGRAInto := GetProcedureAddress(LibwebpHandle,'WebPDecodeBGRAInto');
    @WebPDecodeRGBInto := GetProcedureAddress(LibwebpHandle,'WebPDecodeRGBInto');
    @WebPDecodeBGRInto := GetProcedureAddress(LibwebpHandle,'WebPDecodeBGRInto');

    @WebPInitDecBufferInternal := GetProcedureAddress(LibwebpHandle,'WebPInitDecBufferInternal');
    @WebPFreeDecBuffer := GetProcedureAddress(LibwebpHandle,'WebPFreeDecBuffer');
    @WebPINewDecoder := GetProcedureAddress(LibwebpHandle,'WebPINewDecoder');
    @WebPIDelete := GetProcedureAddress(LibwebpHandle,'WebPIDelete');
    @WebPIAppend := GetProcedureAddress(LibwebpHandle,'WebPIAppend');
    @WebPIUpdate := GetProcedureAddress(LibwebpHandle,'WebPIUpdate');
    @WebPIDecGetRGB := GetProcedureAddress(LibwebpHandle,'WebPIDecGetRGB');

    @WebPGetFeaturesInternal := GetProcedureAddress(LibwebpHandle,'WebPGetFeaturesInternal');
    @WebPInitDecoderConfigInternal := GetProcedureAddress(LibwebpHandle,'WebPInitDecoderConfigInternal');
    @WebPIDecode := GetProcedureAddress(LibwebpHandle,'WebPIDecode');
    @WebPDecode := GetProcedureAddress(LibwebpHandle,'WebPDecode');

    @WebPGetEncoderVersion := GetProcedureAddress(LibwebpHandle,'WebPGetEncoderVersion');
    @WebPEncodeRGB := GetProcedureAddress(LibwebpHandle,'WebPEncodeRGB');
    @WebPEncodeBGR := GetProcedureAddress(LibwebpHandle,'WebPEncodeBGR');
    @WebPEncodeRGBA := GetProcedureAddress(LibwebpHandle,'WebPEncodeRGBA');
    @WebPEncodeBGRA := GetProcedureAddress(LibwebpHandle,'WebPEncodeBGRA');
    @WebPEncodeLosslessRGB := GetProcedureAddress(LibwebpHandle,'WebPEncodeLosslessRGB');
    @WebPEncodeLosslessBGR := GetProcedureAddress(LibwebpHandle,'WebPEncodeLosslessBGR');
    @WebPEncodeLosslessRGBA := GetProcedureAddress(LibwebpHandle,'WebPEncodeLosslessRGBA');
    @WebPEncodeLosslessBGRA := GetProcedureAddress(LibwebpHandle,'WebPEncodeLosslessBGRA');

    @WebPConfigInitInternal := GetProcedureAddress(LibwebpHandle,'WebPConfigInitInternal');
    @WebPConfigLosslessPreset := GetProcedureAddress(LibwebpHandle,'WebPConfigLosslessPreset');
    @WebPValidateConfig := GetProcedureAddress(LibwebpHandle,'WebPValidateConfig');
    @WebPMemoryWriterInit := GetProcedureAddress(LibwebpHandle,'WebPMemoryWriterInit');
    @WebPMemoryWriterClear := GetProcedureAddress(LibwebpHandle,'WebPMemoryWriterClear');
    @WebPMemoryWrite := GetProcedureAddress(LibwebpHandle,'WebPMemoryWrite');
    @WebPPictureInitInternal := GetProcedureAddress(LibwebpHandle,'WebPPictureInitInternal');
    @WebPPictureAlloc := GetProcedureAddress(LibwebpHandle,'WebPPictureAlloc');
    @WebPPictureFree := GetProcedureAddress(LibwebpHandle,'WebPPictureFree');
    @WebPEncode := GetProcedureAddress(LibwebpHandle,'WebPEncode');
  end;
  Exit(True);
end;

procedure UnloadLibwebp;
begin
  if RefCount > 0 then begin
    Dec(RefCount);
    if RefCount = 0 then
      UnloadLibrary(LibwebpHandle);
  end;
end;

// Retrieve features from the bitstream. The *features structure is filled
// with information gathered from the bitstream.
// Returns VP8_STATUS_OK when the features are successfully retrieved. Returns
// VP8_STATUS_NOT_ENOUGH_DATA when more data is needed to retrieve the
// features from headers. Returns error in other cases.
// Note: The following chunk sequences (before the raw VP8/VP8L data) are
// considered valid by this function:
// RIFF + VP8(L)
// RIFF + VP8X + (optional chunks) + VP8(L)
// ALPH + VP8 <-- Not a valid WebP format: only allowed for internal purpose.
// VP8(L)     <-- Not a valid WebP format: only allowed for internal purpose.
function WebPGetFeatures(const data: Puint8_t; data_size: size_t; features: PWebPBitstreamFeatures): VP8StatusCode; inline;
begin
  Result := WebPGetFeaturesInternal(data, data_size, features, WEBP_DECODER_ABI_VERSION);
end;

// Initialize the structure as empty. Must be called before any other use.
// Returns false in case of version mismatch
function WebPInitDecBuffer(buffer: PWebPDecBuffer): int; inline;
begin
   Result := WebPInitDecBufferInternal(buffer, WEBP_DECODER_ABI_VERSION);
end;

// Initialize the configuration as empty. This function must always be
// called first, unless WebPGetFeatures() is to be called.
// Returns false in case of mismatched version.
function WebPInitDecoderConfig(config: PWebPDecoderConfig): int; inline;
begin
  Result := WebPInitDecoderConfigInternal(config, WEBP_DECODER_ABI_VERSION);
end;


// Should always be called, to initialize a fresh WebPConfig structure before
// modification. Returns false in case of version mismatch. WebPConfigInit()
// must have succeeded before using the 'config' object.
// Note that the default values are lossless=0 and quality=75.
function WebPConfigInit(config: PWebPConfig): int; inline;
begin
  Result := WebPConfigInitInternal(config, WEBP_PRESET_DEFAULT, 75, WEBP_ENCODER_ABI_VERSION);
end;

// This function will initialize the configuration according to a predefined
// set of parameters (referred to by 'preset') and a given quality factor.
// This function can be called as a replacement to WebPConfigInit(). Will
// return false in case of error.
function WebPConfigPreset(config: PWebPConfig; preset: WebPPreset; quality: float): int; inline;
begin
  Result := WebPConfigInitInternal(config, preset, quality, WEBP_ENCODER_ABI_VERSION);
end;

// Should always be called, to initialize the structure. Returns false in case
// of version mismatch. WebPPictureInit() must have succeeded before using the
// 'picture' object.
// Note that, by default, use_argb is false and colorspace is WEBP_YUV420.
function WebPPictureInit(picture: PWebPPicture): int; inline;
begin
  Result := WebPPictureInitInternal(picture, WEBP_ENCODER_ABI_VERSION);
end;

end.

