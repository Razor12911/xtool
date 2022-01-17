{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory of
  this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}

{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}

// The original source file can be found here:
// https://bitbucket.org/egrange/dwscript/src

   add edx, 128
   add eax, 128

   movq  mm1, [edx-120]
   movq  mm4, [edx-96]
   movq  mm3, [edx-104]
   pxor  mm1, [edx-80]
   movq  mm5, [edx+16]
   pxor  mm1, [edx]
   movq  mm2, [edx-112]
   pxor  mm1, [edx+40]
   pxor  mm1, [edx-40]
   movq  mm0, [edx-128]
   movq  mm6, mm1
   pxor  mm4, [edx-56]
   movq  [ecx+8], mm1
   psrlq mm6, 63
   pxor  mm4, [edx+24]
   pxor  mm4, [edx+64]
   pxor  mm4, [edx-16]
   psllq mm1, 1
   pxor  mm2, [edx+48]
   por   mm1, mm6
   movq  mm6, [edx-88]
   pxor  mm1, mm4
   pxor  mm2, [edx-32]
   pxor  mm2, [edx-72]
   pxor  mm6, mm1
   movq  mm7, mm6
   psrlq mm7, 28
   psllq mm6, 36
   por   mm6, mm7
   pxor  mm2, [edx+8]
   movq  [eax], mm6
   movq  mm6, [edx+32]
   movq  mm7, mm4
   psrlq mm7, 63
   psllq mm4, 1
   pxor  mm0, mm6
   por   mm4, mm7
   pxor  mm4, mm2
   pxor  mm5, mm4
   movq  mm7, mm5
   pxor  mm0, [edx-8]
   psllq mm5, 21
   psrlq mm7, 43
   pxor  mm6, mm1
   por   mm5, mm7
   movq  [eax-104], mm5
   movq  mm5, [edx-48]
   pxor  mm0, mm5
   movq  mm7, mm6
   psrlq mm7, 46
   psllq mm6, 18
   por   mm6, mm7
   movq  [eax-16], mm6
   movq  mm6, [edx+56]
   pxor  mm5, mm1
   movq  mm7, mm5
   pxor  mm3, mm6
   psllq mm5, 3
   psrlq mm7, 61
   pxor  mm3, [edx+16]
   pxor  mm3, [edx-24]
   por   mm5, mm7
   pxor  mm6, mm4
   pxor  mm0, [edx-88]
   movq  mm7, mm6
   psrlq mm7, 8
   movq  [eax-72], mm5
   movq  mm5, mm2
   psllq mm2, 1
   psllq mm6, 56
   psrlq mm5, 63
   por   mm6, mm7
   por   mm2, mm5
   pxor  mm2, mm0
   movq  [eax+24], mm6
   movq  mm5, [edx-120]
   movq  mm6, mm0
   psllq mm0, 1
   pxor  mm5, mm2
   pxor  mm3, [edx-64]
   psrlq mm6, 63
   por   mm0, mm6
   movq  mm6, [edx-64]
   movq  mm7, mm5
   psllq mm5, 1
   psrlq mm7, 63
   pxor  mm6, mm4
   por   mm5, mm7
   pxor  mm0, mm3
   movq  mm7, mm6
   movq  [eax-48], mm5
   movq  mm5, [edx]
   psllq mm6, 55
   psrlq mm7, 9
   por   mm6, mm7
   movq  [eax+40], mm6
   movq  mm6, [edx-40]
   pxor  mm5, mm2
   movq  mm7, mm5
   psllq mm5, 45
   psrlq mm7, 19
   pxor  mm6, mm2
   por   mm5, mm7
   movq  [eax-64], mm5
   movq  mm5, [edx+40]
   movq  mm7, mm6
   pxor  mm5, mm2
   psllq mm6, 10
   psrlq mm7, 54
   por   mm6, mm7
   movq  [eax+8], mm6
   movq  mm6, [edx-96]
   movq  mm7, mm3
   psrlq mm7, 63
   psllq mm3, 1
   por   mm3, mm7
   movq  mm7, mm5
   psllq mm5, 2
   psrlq mm7, 62
   por   mm5, mm7
   movq  [eax+64], mm5
   movq  mm5, [edx+24]
   pxor  mm6, mm0
   movq  mm7, mm6
   psrlq mm7, 37
   psllq mm6, 27
   por   mm6, mm7
   movq  [eax-8], mm6
   pxor  mm5, mm0
   movq  mm6, [edx-16]
   movq  mm7, mm5
   psllq mm5, 8
   pxor  mm3, [ecx+8]
   psrlq mm7, 56
   pxor  mm6, mm0
   por   mm5, mm7
   movq  [eax-24], mm5
   movq  mm7, mm6
   psllq mm6, 39
   movq  mm5, [edx-112]
   psrlq mm7, 25
   por   mm6, mm7
   movq  [eax+48], mm6
   movq  mm6, [edx-24]
   pxor  mm5, mm3
   movq  mm7, mm5
   psrlq mm7, 2
   psllq mm5, 62
   por   mm5, mm7
   movq  [eax+32], mm5
   movq  mm5, [edx-104]
   pxor  mm6, mm4
   movq  mm7, mm6
   psrlq mm7, 39
   psllq mm6, 25
   por   mm6, mm7
   pxor  mm5, mm4
   movq  [eax-32], mm6
   movq  mm6, [edx-128]
   pxor  mm6, mm1
   movq  mm4, mm6
   movq  [eax-128], mm6
   movq  mm4, mm6
   movq  mm6, [edx-8]
   movq  mm7, mm5
   psrlq mm7, 36
   psllq mm5, 28
   pxor  mm6, mm1
   por   mm5, mm7
   movq  mm7, mm6
   psrlq mm7, 23
   movq  mm1, mm5
   movq  [eax-88], mm5
   movq  mm5, [edx-56]
   pxor  mm5, mm0
   psllq mm6, 41
   por   mm6, mm7
   movq  [eax+56], mm6
   movq  mm6, [edx+48]
   pxor  mm6, mm3
   movq  mm7, mm5
   psrlq mm7, 44
   psllq mm5, 20
   por   mm5, mm7
   movq  [eax-80], mm5
   pandn mm1, mm5
   movq  mm5, [edx-32]
   movq  mm7, mm6
   psrlq mm7, 3
   psllq mm6, 61
   por   mm6, mm7
   pxor  mm1, mm6
   movq  [eax-56], mm6
   movq  mm6, [edx+8]
   movq  [edx-56], mm1
   movq  mm1, [eax-112]
   pxor  mm5, mm3
   movq  mm7, mm5
   psllq mm5, 43
   psrlq mm7, 21
   pxor  mm6, mm3
   por   mm5, mm7
   movq  mm1, mm5
   movq  mm5, [edx-80]
   pxor  mm5, mm2
   movq  mm2, [eax-104]
   movq  mm7, mm6
   psrlq mm7, 49
   psllq mm6, 15
   por   mm6, mm7
   movq  [eax+16], mm6
   movq  mm6, [edx+64]
   movq  [eax-96], mm6
   movq  mm7, mm5
   psrlq mm7, 20
   psllq mm5, 44
   pxor  mm6, mm0
   por   mm5, mm7
   movq  mm7, mm6
   psrlq mm7, 50
   psllq mm6, 14
   por   mm6, mm7
   pandn mm2, mm6
   movq  mm0, mm5
   pandn mm0, mm1
   pxor  mm2, mm1
   pandn mm1, [eax-104]
   movq  [edx-112], mm2
   pandn mm4, mm5
   pxor  mm1, mm5
   movq  [eax-120], mm5
   movq  mm2, [eax-40]
   movq  [edx-120], mm1
   movq  mm5, [edx-72]
   movq  mm1, [eax-64]
   pxor  mm4, mm6
   movq  [edx-96], mm4
   pxor  mm5, mm3
   movq  mm4, [eax-88]
   movq  mm7, mm5
   movq  mm3, mm6
   pxor  mm0, [eax-128]
   movq  [edx-128], mm0
   movq  mm6, [eax-72]
   psllq mm5, 6
   psrlq mm7, 58
   movq  mm0, [eax-56]
   por   mm5, mm7
   movq  mm2, mm5
   movq  mm5, [eax-80]
   movq  mm7, mm1
   pandn mm7, mm0
   pxor  mm7, mm6
   movq  [edx-72], mm7
   movq  mm7, [eax-72]
   pandn mm6, mm1
   pxor  mm6, mm5
   pandn mm0, mm4
   pandn mm5, mm7
   movq  mm7, [eax]
   pxor  mm5, mm4
   movq  mm4, [eax-24]
   movq  [edx-80], mm6
   movq  mm6, [eax-48]
   movq  [edx-88], mm5
   movq  mm5, mm1
   movq  mm1, [eax-16]
   pxor  mm0, mm5
   movq  mm5, mm1
   pandn mm3, [eax-128]
   pxor  mm3, [eax-104]
   movq  [edx-64], mm0
   movq  mm0, [eax+8]
   movq  [edx-104], mm3
   movq  mm3, [eax-32]
   pandn mm6, mm2
   pxor  mm6, mm5
   movq  [edx-16], mm6
   movq  mm6, [eax+56]
   pandn mm3, mm4
   pxor  mm3, mm2
   movq  [edx-40], mm3
   movq  mm3, [eax-32]
   pandn mm5, [eax-48]
   pxor  mm5, mm4
   movq  [edx-24], mm5
   pandn mm7, mm0
   movq  mm5, [eax+16]
   pandn mm4, mm1
   pxor  mm4, mm3
   movq  [edx-32], mm4
   movq  mm4, [eax+40]
   movq  mm1, mm5
   movq  mm5, [eax+48]
   pandn mm5, mm6
   pxor  mm5, mm4
   pandn mm2, mm3
   movq  mm3, [eax-8]
   movq  [edx+40], mm5
   movq  mm5, [eax+24]
   pxor  mm7, mm3
   movq  [edx-8], mm7
   movq  mm7, [eax+64]
   pxor  mm2, [eax-48]
   movq  [edx-48], mm2
   movq  mm2, mm5
   pandn mm2, mm3
   pxor  mm2, mm1
   movq  [edx+16], mm2
   pandn mm3, [eax]
   movq  mm2, mm5
   movq  mm5, [eax+48]
   pandn mm6, mm7
   pxor  mm6, mm5
   movq  [edx+48], mm6
   pandn mm1, mm2
   movq  mm6, [eax+32]
   pxor  mm1, mm0
   pxor  mm3, mm2
   movq  [edx+24], mm3
   pandn mm0, [eax+16]
   pxor  mm0, [eax]
   movq  mm3, mm4
   movq  [edx+8], mm1
   movq  [edx], mm0
   movq  mm0, mm6
   movq  mm1, [eax+56]
   pandn mm4, mm5
   pxor  mm4, mm0
   pandn mm0, mm3
   pxor  mm0, mm7
   movq  [edx+32], mm4
   pandn mm7, mm6
   pxor  mm7, mm1
   movq  [edx+56], mm7
   movq  [edx+64], mm0

