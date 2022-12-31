unit FLACDLL;

interface

uses
  InitCode,
  Utils, LibImport,
  WinAPI.Windows,
  System.SysUtils, System.Classes;

const
  FLAC__MAX_CHANNELS = 8;
  FLAC__MAX_FIXED_ORDER = 4;
  FLAC__MAX_LPC_ORDER = 32;

type
  TFLAC__FrameHeader = record
    blocksize, sample_rate, channels: Cardinal;
    channel_assignment: Integer;
    bits_per_sample: Cardinal;
    case number_type: Integer of
      0:
        (frame_number: Cardinal; crc1: Byte);
      1:
        (sample_number: UInt64; crc2: Byte);
  end;

  PFLAC__EntropyCodingMethod_PartitionedRiceContents = ^
    TFLAC__EntropyCodingMethod_PartitionedRiceContents;

  TFLAC__EntropyCodingMethod_PartitionedRiceContents = record
    parameters, raw_bits: PCardinal;
    capacity_by_order: Cardinal;
  end;

  TFLAC__EntropyCodingMethod_PartitionedRice = record
    order: Cardinal;
    contents: PFLAC__EntropyCodingMethod_PartitionedRiceContents;
  end;

  TFLAC__EntropyCodingMethod = record
    case ftype: Integer of
      0:
        (partitioned_rice: TFLAC__EntropyCodingMethod_PartitionedRice);
  end;

  TFLAC__Subframe_Fixed = record
    entropy_coding_method: TFLAC__EntropyCodingMethod;
    order: Cardinal;
    warmup: array [0 .. FLAC__MAX_CHANNELS - 1] of Integer;
    residual: PInteger;
  end;

  TFLAC__Subframe_LPC = record
    entropy_coding_method: TFLAC__EntropyCodingMethod;
    order, qlp_coeff_precision: Cardinal;
    quantization_level: Integer;
    qlp_coeff, warmup: array [0 .. FLAC__MAX_LPC_ORDER - 1] of Integer;
    residual: PInteger;
  end;

  TFLAC__Subframe = record
    case ftype: Integer of
      0:
        (constant: Integer; wb1: Cardinal);
      1:
        (fixed: TFLAC__Subframe_Fixed; wb2: Cardinal);
      2:
        (lpc: TFLAC__Subframe_LPC; wb3: Cardinal);
      3:
        (verbatim: PInteger; wb4: Cardinal);
  end;

  PFLAC__Frame = ^TFLAC__Frame;

  TFLAC__Frame = record
    header: TFLAC__FrameHeader;
    subframes: array [0 .. FLAC__MAX_CHANNELS - 1] of TFLAC__Subframe;
    footer: Word;
  end;

var
  FLAC__stream_encoder_new: function: Pointer cdecl;
  FLAC__stream_encoder_set_verify: function(encoder: Pointer; value: Boolean)
    : Boolean cdecl;
  FLAC__stream_encoder_set_compression_level: function(encoder: Pointer;
    value: Cardinal): Boolean cdecl;
  FLAC__stream_encoder_set_channels: function(encoder: Pointer; value: Cardinal)
    : Boolean cdecl;
  FLAC__stream_encoder_set_bits_per_sample: function(encoder: Pointer;
    value: Cardinal): Boolean cdecl;
  FLAC__stream_encoder_set_sample_rate: function(encoder: Pointer;
    value: Cardinal): Boolean cdecl;
  FLAC__stream_encoder_set_total_samples_estimate: function(encoder: Pointer;
    value: UInt64): Boolean cdecl;
  FLAC__stream_encoder_init_stream: function(encoder: Pointer;
    write_callback, seek_callback, tell_callback, metadata_callback: Pointer;
    client_data: Pointer): Integer cdecl;
  FLAC__stream_encoder_init_file: function(encoder: Pointer;
    filename: PAnsiChar; progress_callback: Pointer; client_data: Pointer)
    : Integer cdecl;
  FLAC__stream_encoder_process_interleaved: function(encoder: Pointer;
    const buffer; samples: Cardinal): Boolean cdecl;
  FLAC__stream_encoder_finish: function(encoder: Pointer): Boolean cdecl;
  FLAC__stream_encoder_delete: procedure(encoder: Pointer)cdecl;
  FLAC__stream_decoder_new: function: Pointer cdecl;
  FLAC__stream_decoder_init_stream: function(decoder: Pointer;
    read_callback, seek_callback, tell_callback, length_callback, eof_callback,
    write_callback, metadata_callback, error_callback: Pointer;
    client_data: Pointer): Integer cdecl;
  FLAC__stream_decoder_init_file: function(decoder: Pointer;
    filename: PAnsiChar; write_callback, metadata_callback, error_callback
    : Pointer; client_data: Pointer): Integer cdecl;
  FLAC__stream_decoder_get_channels: function(decoder: Pointer): Cardinal cdecl;
  FLAC__stream_decoder_get_bits_per_sample: function(decoder: Pointer)
    : Cardinal cdecl;
  FLAC__stream_decoder_process_until_end_of_stream: function(decoder: Pointer)
    : Boolean cdecl;
  FLAC__stream_decoder_finish: function(encoder: Pointer): Boolean cdecl;
  FLAC__stream_decoder_delete: procedure(encoder: Pointer)cdecl;
  DLLLoaded: Boolean = False;

implementation

var
  Lib: TLibImport;

procedure Init;
begin
  Lib := TLibImport.Create
    (ExpandPath(PluginsPath + 'libFLAC_dynamic.dll', True));
  if Lib.Loaded then
  begin
    @FLAC__stream_encoder_new := Lib.GetProcAddr('FLAC__stream_encoder_new');
    @FLAC__stream_encoder_set_verify :=
      Lib.GetProcAddr('FLAC__stream_encoder_set_verify');
    @FLAC__stream_encoder_set_channels :=
      Lib.GetProcAddr('FLAC__stream_encoder_set_channels');
    @FLAC__stream_encoder_set_compression_level :=
      Lib.GetProcAddr('FLAC__stream_encoder_set_compression_level');
    @FLAC__stream_encoder_set_bits_per_sample :=
      Lib.GetProcAddr('FLAC__stream_encoder_set_bits_per_sample');
    @FLAC__stream_encoder_set_sample_rate :=
      Lib.GetProcAddr('FLAC__stream_encoder_set_sample_rate');
    @FLAC__stream_encoder_set_total_samples_estimate :=
      Lib.GetProcAddr('FLAC__stream_encoder_set_total_samples_estimate');
    @FLAC__stream_encoder_init_stream :=
      Lib.GetProcAddr('FLAC__stream_encoder_init_stream');
    @FLAC__stream_encoder_init_file :=
      Lib.GetProcAddr('FLAC__stream_encoder_init_file');
    @FLAC__stream_encoder_process_interleaved :=
      Lib.GetProcAddr('FLAC__stream_encoder_process_interleaved');
    @FLAC__stream_encoder_finish :=
      Lib.GetProcAddr('FLAC__stream_encoder_finish');
    @FLAC__stream_encoder_delete :=
      Lib.GetProcAddr('FLAC__stream_encoder_delete');
    @FLAC__stream_decoder_new := Lib.GetProcAddr('FLAC__stream_decoder_new');
    @FLAC__stream_decoder_init_stream :=
      Lib.GetProcAddr('FLAC__stream_decoder_init_stream');
    @FLAC__stream_decoder_init_file :=
      Lib.GetProcAddr('FLAC__stream_decoder_init_file');
    @FLAC__stream_decoder_get_channels :=
      Lib.GetProcAddr('FLAC__stream_decoder_get_channels');
    @FLAC__stream_decoder_get_bits_per_sample :=
      Lib.GetProcAddr('FLAC__stream_decoder_get_bits_per_sample');
    @FLAC__stream_decoder_process_until_end_of_stream :=
      Lib.GetProcAddr('FLAC__stream_decoder_process_until_end_of_stream');
    @FLAC__stream_decoder_finish :=
      Lib.GetProcAddr('FLAC__stream_decoder_finish');
    @FLAC__stream_decoder_delete :=
      Lib.GetProcAddr('FLAC__stream_decoder_delete');
    DLLLoaded := Assigned(FLAC__stream_encoder_new) and
      Assigned(FLAC__stream_decoder_new);
  end;
end;

procedure Deinit;
begin
  Lib.Free;
end;

initialization

Init;

finalization

Deinit;

end.
