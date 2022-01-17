;{ ****************************************************************************** }
;{ * https://zpascal.net                                                        * }
;{ * https://github.com/PassByYou888/zAI                                        * }
;{ * https://github.com/PassByYou888/ZServer4D                                  * }
;{ * https://github.com/PassByYou888/PascalString                               * }
;{ * https://github.com/PassByYou888/zRasterization                             * }
;{ * https://github.com/PassByYou888/CoreCipher                                 * }
;{ * https://github.com/PassByYou888/zSound                                     * }
;{ * https://github.com/PassByYou888/zChinese                                   * }
;{ * https://github.com/PassByYou888/zExpression                                * }
;{ * https://github.com/PassByYou888/zGameWare                                  * }
;{ * https://github.com/PassByYou888/zAnalysis                                  * }
;{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
;{ * https://github.com/PassByYou888/zTranslate                                 * }
;{ * https://github.com/PassByYou888/InfiniteIoT                                * }
;{ * https://github.com/PassByYou888/FastMD5                                    * }
;{ ****************************************************************************** }


; MD5_Transform-x64
; MD5 transform routine oprimized for x64 processors
; Copyright 2018 Ritlabs, SRL
; The 64-bit version is written by Maxim Masiutin <max@ritlabs.com>

; The main advantage of this 64-bit version is that
; it loads 64 bytes of hashed message into 8 64-bit registers 
; (RBP, R8, R9, R10, R11, R12, R13, R14) at the beginning,
; to avoid excessive memory load operations 
; througout the routine.

; To operate with 32-bit values store in higher bits
; of a 64-bit register (bits 32-63) uses "Ror" by 32;
; 8 macro variables (M1-M8) are used to keep record
; or corrent state of whether the register has been
; Ror'ed or not.

; It also has an ability to use Lea instruction instead
; of two sequental Adds (uncomment UseLea=1), but it is 
; slower on Skylake processors. Also, Intel in the 
; Optimization Reference Maual discourages us of
; Lea as a replacement of two adds, since it is slower 
; on the Atom processors.

; MD5_Transform-x64 is released under a dual license, 
; and you may choose to use it under either the 
; Mozilla Public License 2.0 (MPL 2.1, available from
; https://www.mozilla.org/en-US/MPL/2.0/) or the 
; GNU Lesser General Public License Version 3, 
; dated 29 June 2007 (LGPL 3, available from
; https://www.gnu.org/licenses/lgpl.html).

; MD5_Transform-x64 is based 
; on the following code by Peter Sawatzki. 

; The original notice by Peter Sawatzki follows.

; ==============================================================
;
; MD5_386.Asm   -  386 optimized helper routine for calculating
;                  MD Message-Digest values
; written 2/2/94 by
;
; Peter Sawatzki
; Buchenhof 3
; D58091 Hagen, Germany Fed Rep
;
; EMail: Peter@Sawatzki.de
; EMail: 100031.3002@compuserve.com
; WWW:   http://www.sawatzki.de
;
;
; original C Source was found in Dr. Dobbs Journal Sep 91
; MD5 algorithm from RSA Data Security, Inc.



           .CODE


; You can compile this code using Microsoft Macro Assembler 
;     ml64.exe /c md5_64.asm



; Uncomment the line below if you wish to have 
; a "Lea" instruction instead of two subsequent "Add".

; UseLea=1



; The AA macro adds r to ac to a and stores result to r
; r and a can be either 32-bit (for the "Add" version) 
; or 64-bit (for the "Lea" version)

AA Macro r32,r64,ac,a32,a64
  IFDEF UseLea
     Lea r64, [r64+ac+a64]
  ELSE
     Add r32, ac
     Add r32, a32
  ENDIF
EndM

; The JJ macro adds value from state buffer to the "a" register
; The "a" register can be either 32-bit (for the "Add" version) 
; or 64-bit (for "Lea") - in this case it is passed as "r"

JJ Macro a,x,ac,r
IFE x
  IF M1
   Ror RBp, 32
   M1=0
  ENDIF
   AA a, r, ac, EBp, RBp
ENDIF
IFE x-1
  IFE M1
   Ror RBp, 32
   M1=1
  ENDIF
   AA a, r, ac, EBp, RBp
ENDIF
IFE x-2
  IF M2
   Ror R8, 32
   M2=0
  ENDIF
   AA a, r, ac, R8d, R8
ENDIF
IFE x-3
  IFE M2
   Ror R8, 32
   M2=1
  ENDIF
   AA a, r, ac, R8d, R8
ENDIF
IFE x-4
  IF M3
   Ror R9, 32
   M3=0
  ENDIF
   AA a, r, ac, R9d, R9
ENDIF
IFE x-5
  IFE M3
   Ror R9, 32
   M3=1
  ENDIF
   AA a, r, ac, R9d, R9
ENDIF
IFE x-6
  IF M4
   Ror R10, 32
   M4=0
  ENDIF
   AA a, r, ac, R10d, R10
ENDIF
IFE x-7
  IFE M4
   Ror R10, 32
   M4=1
  ENDIF
   AA a, r, ac, R10d, R10
ENDIF
IFE x-8
  IF M5
   Ror R11, 32
   M5=0
  ENDIF
   AA a, r, ac, R11d, R11
ENDIF
IFE x-9
  IFE M5
   Ror R11, 32
   M5=1
  ENDIF
   AA a, r, ac, R11d, R11
ENDIF
IFE x-10
  IF M6
   Ror R12, 32
   M6=0
  ENDIF
   AA a, r, ac, R12d, R12
ENDIF
IFE x-11
  IFE M6
   Ror R12, 32
   M6=1
  ENDIF
   AA a, r, ac, R12d, R12
ENDIF
IFE x-12
  IF M7
   Ror R13, 32
   M7=0
  ENDIF
   AA a, r, ac, R13d, R13
ENDIF
IFE x-13
  IFE M7
   Ror R13, 32
   M7=1
  ENDIF
   AA a, r, ac, R13d, R13
ENDIF
IFE x-14
  IF M8
   Ror R14, 32
   M8=0
  ENDIF
   AA a, r, ac, R14d, R14
ENDIF
IFE x-15
  IFE M8
   Ror R14, 32
   M8=1
  ENDIF
   AA a, r, ac, R14d, R14
ENDIF
EndM


FF Macro a,b,c,d,x,s,ac,r
; a:= ROL (a+x+ac + (b And c Or Not b And d), s) + b
  JJ a, x, ac, r
  Mov ESI, b
  Not ESI
  And ESI, d
  Mov EDI, c
  And EDI, b
  Or  ESI, EDI
  Add a, ESI
  Rol a, s
  Add a, b
EndM

GG Macro a,b,c,d,x,s,ac,r
; a:= ROL (a+x+ac + (b And d Or c And Not d), s) + b
  JJ a, x, ac, r
  Mov ESI, d
  Not ESI
  And ESI, c
  Mov EDI, d
  And EDI, b
  Or  ESI, EDI
  Add a, ESI
  Rol a, s
  Add a, b
EndM

HH Macro a,b,c,d,x,s,ac,r
; a:= ROL (a+x+ac + (b Xor c Xor d), s) + b
  JJ a, x, ac, r
  Mov ESI, d
  Xor ESI, c
  Xor ESI, b
  Add a, ESI
  Rol a, s
  Add a, b
EndM

II Macro a,b,c,d,x,s,ac,r
; a:= ROL (a+x+ac + (c Xor (b Or Not d)), s) + b
  JJ a, x, ac, r
  Mov ESI, d
  Not ESI
  Or  ESI, b
  Xor ESI, c
  Add a, ESI
  Rol a, s
  Add a, b
EndM

MD5_Transform Proc
Public MD5_Transform

; save registers that the caller requires to be restored
  Push RBx
  Push RSi
  Push RDi

  Push RBp
  Push R12
  Push R13
  Push R14

; First parameter is passed in RCX, Second - in RDX

; State   - in RCX
; Message - in RDX

  M1 = 0
  M2 = 0
  M3 = 0
  M4 = 0
  M5 = 0
  M6 = 0
  M7 = 0
  M8 = 0

  Mov R14, RDX ; Now the message buffer offset is in R14

  Mov RSi, Rcx ; Now state structure offset is in RSi
  Push Rsi     ; State -> Stack
  Mov EAx, [RSi]
  Mov EBx, [RSi+4]
  Mov ECx, [RSi+8]
  Mov EDx, [RSi+12]

  Mov RBP, [R14+4*0]
  FF EAx,EBx,ECx,EDx,  0,  7, 0d76aa478h, RAx  ; 1
  FF EDx,EAx,EBx,ECx,  1, 12, 0e8c7b756h, RDx  ; 2
  Mov R8,  [R14+4*2]
  FF ECx,EDx,EAx,EBx,  2, 17, 0242070dbh, RCx  ; 3
  FF EBx,ECx,EDx,EAx,  3, 22, 0c1bdceeeh, RBx  ; 4
  Mov R9,  [R14+4*4]
  FF EAx,EBx,ECx,EDx,  4,  7, 0f57c0fafh, RAx  ; 5
  FF EDx,EAx,EBx,ECx,  5, 12, 04787c62ah, RDx  ; 6
  Mov R10, [R14+4*6]
  FF ECx,EDx,EAx,EBx,  6, 17, 0a8304613h, RCx  ; 7
  FF EBx,ECx,EDx,EAx,  7, 22, 0fd469501h, RBx  ; 8
  Mov R11, [R14+4*8]
  FF EAx,EBx,ECx,EDx,  8,  7, 0698098d8h, RAx  ; 9
  FF EDx,EAx,EBx,ECx,  9, 12, 08b44f7afh, RDx  ; 10
  Mov R12, [R14+4*10]
  FF ECx,EDx,EAx,EBx, 10, 17, 0ffff5bb1h, RCx  ; 11
  FF EBx,ECx,EDx,EAx, 11, 22, 0895cd7beh, RBx  ; 12
  Mov R13, [R14+4*12]
  FF EAx,EBx,ECx,EDx, 12,  7, 06b901122h, RAx  ; 13
  FF EDx,EAx,EBx,ECx, 13, 12, 0fd987193h, RDx  ; 14
  Mov R14, [R14+4*14]
  FF ECx,EDx,EAx,EBx, 14, 17, 0a679438eh, RCx  ; 15
  FF EBx,ECx,EDx,EAx, 15, 22, 049b40821h, RBx  ; 16

  GG EAx,EBx,ECx,EDx,  1,  5, 0f61e2562h, RAx  ; 17
  GG EDx,EAx,EBx,ECx,  6,  9, 0c040b340h, RDx  ; 18
  GG ECx,EDx,EAx,EBx, 11, 14, 0265e5a51h, RCx  ; 19
  GG EBx,ECx,EDx,EAx,  0, 20, 0e9b6c7aah, RBx  ; 20
  GG EAx,EBx,ECx,EDx,  5,  5, 0d62f105dh, RAx  ; 21
  GG EDx,EAx,EBx,ECx, 10,  9, 002441453h, RDx  ; 22
  GG ECx,EDx,EAx,EBx, 15, 14, 0d8a1e681h, RCx  ; 23
  GG EBx,ECx,EDx,EAx,  4, 20, 0e7d3fbc8h, RBx  ; 24
  GG EAx,EBx,ECx,EDx,  9,  5, 021e1cde6h, RAx  ; 25
  GG EDx,EAx,EBx,ECx, 14,  9, 0c33707d6h, RDx  ; 26
  GG ECx,EDx,EAx,EBx,  3, 14, 0f4d50d87h, RCx  ; 27
  GG EBx,ECx,EDx,EAx,  8, 20, 0455a14edh, RBx  ; 28
  GG EAx,EBx,ECx,EDx, 13,  5, 0a9e3e905h, RAx  ; 29
  GG EDx,EAx,EBx,ECx,  2,  9, 0fcefa3f8h, RDx  ; 30
  GG ECx,EDx,EAx,EBx,  7, 14, 0676f02d9h, RCx  ; 31
  GG EBx,ECx,EDx,EAx, 12, 20, 08d2a4c8ah, RBx  ; 32

  HH EAx,EBx,ECx,EDx,  5,  4, 0fffa3942h, RAx  ; 33
  HH EDx,EAx,EBx,ECx,  8, 11, 08771f681h, RDx  ; 34
  HH ECx,EDx,EAx,EBx, 11, 16, 06d9d6122h, RCx  ; 35
  HH EBx,ECx,EDx,EAx, 14, 23, 0fde5380ch, RBx  ; 36
  HH EAx,EBx,ECx,EDx,  1,  4, 0a4beea44h, RAx  ; 37
  HH EDx,EAx,EBx,ECx,  4, 11, 04bdecfa9h, RDx  ; 38
  HH ECx,EDx,EAx,EBx,  7, 16, 0f6bb4b60h, RCx  ; 39
  HH EBx,ECx,EDx,EAx, 10, 23, 0bebfbc70h, RBx  ; 40
  HH EAx,EBx,ECx,EDx, 13,  4, 0289b7ec6h, RAx  ; 41
  HH EDx,EAx,EBx,ECx,  0, 11, 0eaa127fah, RDx  ; 42
  HH ECx,EDx,EAx,EBx,  3, 16, 0d4ef3085h, RCx  ; 43
  HH EBx,ECx,EDx,EAx,  6, 23, 004881d05h, RBx  ; 44
  HH EAx,EBx,ECx,EDx,  9,  4, 0d9d4d039h, RAx  ; 45
  HH EDx,EAx,EBx,ECx, 12, 11, 0e6db99e5h, RDx  ; 46
  HH ECx,EDx,EAx,EBx, 15, 16, 01fa27cf8h, RCx  ; 47
  HH EBx,ECx,EDx,EAx,  2, 23, 0c4ac5665h, RBx  ; 48

  II EAx,EBx,ECx,EDx,  0,  6, 0f4292244h, RAx  ; 49
  II EDx,EAx,EBx,ECx,  7, 10, 0432aff97h, RDx  ; 50
  II ECx,EDx,EAx,EBx, 14, 15, 0ab9423a7h, RCx  ; 51
  II EBx,ECx,EDx,EAx,  5, 21, 0fc93a039h, RBx  ; 52
  II EAx,EBx,ECx,EDx, 12,  6, 0655b59c3h, RAx  ; 53
  II EDx,EAx,EBx,ECx,  3, 10, 08f0ccc92h, RDx  ; 54
  II ECx,EDx,EAx,EBx, 10, 15, 0ffeff47dh, RCx  ; 55
  II EBx,ECx,EDx,EAx,  1, 21, 085845dd1h, RBx  ; 56
  II EAx,EBx,ECx,EDx,  8,  6, 06fa87e4fh, RAx  ; 57
  II EDx,EAx,EBx,ECx, 15, 10, 0fe2ce6e0h, RDx  ; 58
  II ECx,EDx,EAx,EBx,  6, 15, 0a3014314h, RCx  ; 59
  II EBx,ECx,EDx,EAx, 13, 21, 04e0811a1h, RBx  ; 60
  II EAx,EBx,ECx,EDx,  4,  6, 0f7537e82h, RAx  ; 61
  II EDx,EAx,EBx,ECx, 11, 10, 0bd3af235h, RDx  ; 62
  II ECx,EDx,EAx,EBx,  2, 15, 02ad7d2bbh, RCx  ; 63
  II EBx,ECx,EDx,EAx,  9, 21, 0eb86d391h, RBx  ; 64

  Pop RSi            ; get State pointer from stack
  Add [RSi],    EAx
  Add [RSi+4],  EBx
  Add [RSi+8],  ECx
  Add [RSi+12], EDx

; restore volatile registers
  Pop R14
  Pop R13
  Pop R12
  Pop RBp

  Pop RDi
  Pop RSi
  Pop RBx

  Ret
MD5_Transform EndP

  End

; That's All Folks!
