unit LZMADLL;

interface

uses
  WinAPI.Windows,
  System.SysUtils;

type
  PFL2_inBuffer = ^FL2_inBuffer;

  FL2_inBuffer = record
    src: Pointer;
    size: size_t;
    pos: size_t;
  end;

  PFL2_outBuffer = ^FL2_outBuffer;

  FL2_outBuffer = record
    dst: Pointer;
    size: size_t;
    pos: size_t;
  end;

implementation

end.
