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


; This is a 32-bit version of MD5_Transform
; modifief by Maxim Masiutin for Borland 32-bit "register"
; calling convention. For more information on this calling convension, see
; https://en.wikipedia.org/wiki/X86_calling_conventions#Borland_register

; You can compile this code using Microsoft Macro Assembler 
;     ml.exe /c md5_32.asm
; or using Borland Turbo Assembler
;     tasm32.exe /m md5_32.asm

           .386
           .MODEL FLAT
           .CODE

FF Macro a,b,c,d,x,s,ac
; a:= ROL (a+x+ac + (b And c Or Not b And d), s) + b
  Add a, [EBp+(4*x)]
  Add a, ac
  Mov ESi, b
  Not ESi
  And ESi, d
  Mov EDi, c
  And EDi, b
  Or  ESi, EDi
  Add a, ESi
  Rol a, s
  Add a, b
EndM

GG Macro a,b,c,d,x,s,ac
; a:= ROL (a+x+ac + (b And d Or c And Not d), s) + b
  Add a, [EBp+(4*x)]
  Add a, ac
  Mov ESi, d
  Not ESi
  And ESi, c
  Mov EDi, d
  And EDi, b
  Or  ESi, EDi
  Add a, ESi
  Rol a, s
  Add a, b
EndM

HH Macro a,b,c,d,x,s,ac
; a:= ROL (a+x+ac + (b Xor c Xor d), s) + b
  Add a, [EBp+(4*x)]
  Add a, ac
  Mov ESi, d
  Xor ESi, c
  Xor ESi, b
  Add a, ESi
  Rol a, s
  Add a, b
EndM

II Macro a,b,c,d,x,s,ac
; a:= ROL (a+x+ac + (c Xor (b Or Not d)), s) + b
  Add a, [EBp+(4*x)]
  Add a, ac
  Mov ESi, d
  Not ESi
  Or  ESi, b
  Xor ESi, c
  Add a, ESi
  Rol a, s
  Add a, b
EndM

MD5_Transform Proc
Public MD5_Transform

; Use 32-bit Borland Register calling convention
; First Parameter in EAX
; Second Paramerter in EDX

; State buffer offset - in EAx
; Message offset      - in EDx

  Push EBx
  Push ESi
  Push EDi
  Push EBp

  Mov EBp, EDx        ; Now EBp holds Message offset
  Push EAx     
  Mov EDx, [EAx+12]
  Mov ECx, [EAx+8]
  Mov EBx, [EAx+4]
  Mov EAx, [EAx]

  FF EAx,EBx,ECx,EDx,  0,  7, 0d76aa478h  ; 1
  FF EDx,EAx,EBx,ECx,  1, 12, 0e8c7b756h  ; 2
  FF ECx,EDx,EAx,EBx,  2, 17, 0242070dbh  ; 3
  FF EBx,ECx,EDx,EAx,  3, 22, 0c1bdceeeh  ; 4
  FF EAx,EBx,ECx,EDx,  4,  7, 0f57c0fafh  ; 5
  FF EDx,EAx,EBx,ECx,  5, 12, 04787c62ah  ; 6
  FF ECx,EDx,EAx,EBx,  6, 17, 0a8304613h  ; 7
  FF EBx,ECx,EDx,EAx,  7, 22, 0fd469501h  ; 8
  FF EAx,EBx,ECx,EDx,  8,  7, 0698098d8h  ; 9
  FF EDx,EAx,EBx,ECx,  9, 12, 08b44f7afh  ; 10
  FF ECx,EDx,EAx,EBx, 10, 17, 0ffff5bb1h  ; 11
  FF EBx,ECx,EDx,EAx, 11, 22, 0895cd7beh  ; 12
  FF EAx,EBx,ECx,EDx, 12,  7, 06b901122h  ; 13
  FF EDx,EAx,EBx,ECx, 13, 12, 0fd987193h  ; 14
  FF ECx,EDx,EAx,EBx, 14, 17, 0a679438eh  ; 15
  FF EBx,ECx,EDx,EAx, 15, 22, 049b40821h  ; 16

  GG EAx,EBx,ECx,EDx,  1,  5, 0f61e2562h  ; 17
  GG EDx,EAx,EBx,ECx,  6,  9, 0c040b340h  ; 18
  GG ECx,EDx,EAx,EBx, 11, 14, 0265e5a51h  ; 19
  GG EBx,ECx,EDx,EAx,  0, 20, 0e9b6c7aah  ; 20
  GG EAx,EBx,ECx,EDx,  5,  5, 0d62f105dh  ; 21
  GG EDx,EAx,EBx,ECx, 10,  9, 002441453h  ; 22
  GG ECx,EDx,EAx,EBx, 15, 14, 0d8a1e681h  ; 23
  GG EBx,ECx,EDx,EAx,  4, 20, 0e7d3fbc8h  ; 24
  GG EAx,EBx,ECx,EDx,  9,  5, 021e1cde6h  ; 25
  GG EDx,EAx,EBx,ECx, 14,  9, 0c33707d6h  ; 26
  GG ECx,EDx,EAx,EBx,  3, 14, 0f4d50d87h  ; 27
  GG EBx,ECx,EDx,EAx,  8, 20, 0455a14edh  ; 28
  GG EAx,EBx,ECx,EDx, 13,  5, 0a9e3e905h  ; 29
  GG EDx,EAx,EBx,ECx,  2,  9, 0fcefa3f8h  ; 30
  GG ECx,EDx,EAx,EBx,  7, 14, 0676f02d9h  ; 31
  GG EBx,ECx,EDx,EAx, 12, 20, 08d2a4c8ah  ; 32

  HH EAx,EBx,ECx,EDx,  5,  4, 0fffa3942h  ; 33
  HH EDx,EAx,EBx,ECx,  8, 11, 08771f681h  ; 34
  HH ECx,EDx,EAx,EBx, 11, 16, 06d9d6122h  ; 35
  HH EBx,ECx,EDx,EAx, 14, 23, 0fde5380ch  ; 36
  HH EAx,EBx,ECx,EDx,  1,  4, 0a4beea44h  ; 37
  HH EDx,EAx,EBx,ECx,  4, 11, 04bdecfa9h  ; 38
  HH ECx,EDx,EAx,EBx,  7, 16, 0f6bb4b60h  ; 39
  HH EBx,ECx,EDx,EAx, 10, 23, 0bebfbc70h  ; 40
  HH EAx,EBx,ECx,EDx, 13,  4, 0289b7ec6h  ; 41
  HH EDx,EAx,EBx,ECx,  0, 11, 0eaa127fah  ; 42
  HH ECx,EDx,EAx,EBx,  3, 16, 0d4ef3085h  ; 43
  HH EBx,ECx,EDx,EAx,  6, 23, 004881d05h  ; 44
  HH EAx,EBx,ECx,EDx,  9,  4, 0d9d4d039h  ; 45
  HH EDx,EAx,EBx,ECx, 12, 11, 0e6db99e5h  ; 46
  HH ECx,EDx,EAx,EBx, 15, 16, 01fa27cf8h  ; 47
  HH EBx,ECx,EDx,EAx,  2, 23, 0c4ac5665h  ; 48

  II EAx,EBx,ECx,EDx,  0,  6, 0f4292244h  ; 49
  II EDx,EAx,EBx,ECx,  7, 10, 0432aff97h  ; 50
  II ECx,EDx,EAx,EBx, 14, 15, 0ab9423a7h  ; 51
  II EBx,ECx,EDx,EAx,  5, 21, 0fc93a039h  ; 52
  II EAx,EBx,ECx,EDx, 12,  6, 0655b59c3h  ; 53
  II EDx,EAx,EBx,ECx,  3, 10, 08f0ccc92h  ; 54
  II ECx,EDx,EAx,EBx, 10, 15, 0ffeff47dh  ; 55
  II EBx,ECx,EDx,EAx,  1, 21, 085845dd1h  ; 56
  II EAx,EBx,ECx,EDx,  8,  6, 06fa87e4fh  ; 57
  II EDx,EAx,EBx,ECx, 15, 10, 0fe2ce6e0h  ; 58
  II ECx,EDx,EAx,EBx,  6, 15, 0a3014314h  ; 59
  II EBx,ECx,EDx,EAx, 13, 21, 04e0811a1h  ; 60
  II EAx,EBx,ECx,EDx,  4,  6, 0f7537e82h  ; 61
  II EDx,EAx,EBx,ECx, 11, 10, 0bd3af235h  ; 62
  II ECx,EDx,EAx,EBx,  2, 15, 02ad7d2bbh  ; 63
  II EBx,ECx,EDx,EAx,  9, 21, 0eb86d391h  ; 64

  Pop ESi            
  Add [ESi],    EAx
  Add [ESi+4],  EBx
  Add [ESi+8],  ECx
  Add [ESi+12], EDx

; restore the registers to comply to the calling convention
  Pop EBp
  Pop EDi
  Pop ESi
  Pop EBx

  Ret
MD5_Transform EndP

  End
