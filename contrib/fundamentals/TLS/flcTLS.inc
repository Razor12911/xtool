{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLS.inc                                               }
{   Description:      TLS library defines                                      }
{   Last updated:     2020/05/19                                               }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$DEFINE TLS}

{$IFDEF DEBUG}
  {$DEFINE TLS_DEBUG}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE TLS_TEST}
  {.DEFINE TLS_TEST_NO_RANDOM_HELLO}
{$ENDIF}
{$ENDIF}

{$DEFINE TLS_ZLIB_DISABLE}

