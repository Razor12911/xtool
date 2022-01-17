{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcCipher.inc                                            }
{   Description:      Cipher library defines                                   }
{                                                                              }
{******************************************************************************}

{.DEFINE DEBUG}
{.DEFINE TEST}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE CIPHER_TEST}
{$ENDIF}
{$IFDEF PROFILE}
  {$DEFINE CIPHER_PROFILE}
{$ENDIF}
{$ENDIF}

