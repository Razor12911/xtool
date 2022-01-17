{ -------------------------------------------------------------------- }

{
  Modified 04/2015 by David J Butler for inclusion in Fundamentals library.
}

{$INCLUDE ..\..\flcInclude.inc}

{ -------------------------------------------------------------------- }

{
  Modifiied 02/2003 by Sergey A. Galin for Delphi 6+ and Kylix compatibility.
  See README in directory above for more information.
}


{$DEFINE X32}
{$DEFINE Delphi32}
{$DEFINE Delphi}
{$DEFINE Kylix}

{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}

{$DEFINE MAX_MATCH_IS_258}

{$IFNDEF X32}
  {$DEFINE UNALIGNED_OK}  { requires SizeOf(ush) = 2 ! }
{$ENDIF}

{$UNDEF DYNAMIC_CRC_TABLE}
{$UNDEF FASTEST}
{$DEFINE patch112}        { apply patch from the zlib home page }

{ -------------------------------------------------------------------- }

