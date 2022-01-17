{$INCLUDE ..\flcInclude.inc}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE MATHS_TEST}
{$ENDIF}
{$ENDIF}

{$IFDEF ExtendedIsDouble}
  {$DEFINE MFloatIsDouble}
{$ELSE}
  {$DEFINE MFloatIsExtended}
{$ENDIF}

