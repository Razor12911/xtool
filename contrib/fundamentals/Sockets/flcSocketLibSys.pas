{$INCLUDE ..\flcInclude.inc}

// Socket system type
{$IFDEF MSWIN}
  {$DEFINE SOCKETLIB_WIN}
{$ELSE}
  {$IFDEF FREEPASCAL}
    {$DEFINE SOCKETLIB_POSIX_FPC}
  {$ELSE}
    {$DEFINE SOCKETLIB_POSIX_DELPHI}
  {$ENDIF}
{$ENDIF}

unit flcSocketLibSys;

{$IFDEF SOCKETLIB_WIN}
{$INCLUDE flcSocketLibWindows.inc}
{$ENDIF}

{$IFDEF SOCKETLIB_POSIX_DELPHI}
{$INCLUDE flcSocketLibPosixDelphi.inc}
{$ENDIF}

{$IFDEF SOCKETLIB_POSIX_FPC}
{$INCLUDE flcSocketLibPosixFpc.inc}
{$ENDIF}

