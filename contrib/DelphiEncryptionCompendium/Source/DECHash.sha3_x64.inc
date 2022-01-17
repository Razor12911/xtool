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

   push r12
   push r13
   push r14
   add rdx, 128
   add rcx, 128
   // Theta
   mov   rax, [rdx-128]
   xor   rax, [rdx-88]
   xor   rax, [rdx-48]
   xor   rax, [rdx-8]
   xor   rax, [rdx+32]
   mov   [r8], rax
   mov   rax, [rdx-120]
   xor   rax, [rdx-80]
   xor   rax, [rdx-40]
   xor   rax, [rdx]
   xor   rax, [rdx+40]
   mov   [r8+8], rax
   mov   rax, [rdx-112]
   xor   rax, [rdx-72]
   xor   rax, [rdx-32]
   xor   rax, [rdx+8]
   xor   rax, [rdx+48]
   mov   [r8+16], rax
   mov   rax, [rdx-104]
   xor   rax, [rdx-64]
   xor   rax, [rdx-24]
   xor   rax, [rdx+16]
   xor   rax, [rdx+56]
   mov   [r8+24], rax
   mov   rax, [rdx-96]
   xor   rax, [rdx-56]
   xor   rax, [rdx-16]
   xor   rax, [rdx+24]
   xor   rax, [rdx+64]
   mov   [r8+32], rax
   mov   r10, [r8]
   rol   r10, 1
   xor   r10, [r8+24]
   mov   r11, [r8+8]
   rol   r11, 1
   xor   r11, [r8+32]
   mov   r12, [r8+16]
   rol   r12, 1
   xor   r12, [r8]
   mov   r13, [r8+24]
   rol   r13, 1
   xor   r13, [r8+8]
   mov   r14, [r8+32]
   rol   r14, 1
   xor   r14, [r8+16]
   // Rho Pi
   mov   rax, [rdx-128]
   xor   rax, r11
   mov   [rcx-128], rax
   mov   rax, [rdx-80]
   xor   rax, r12
   rol   rax, 44
   mov   [rcx-120], rax
   mov   rax, [rdx-32]
   xor   rax, r13
   rol   rax, 43
   mov   [rcx-112], rax
   mov   rax, [rdx+16]
   xor   rax, r14
   rol   rax, 21
   mov   [rcx-104], rax
   mov   rax, [rdx+64]
   xor   rax, r10
   rol   rax, 14
   mov   [rcx-96], rax
   mov   rax, [rdx-104]
   xor   rax, r14
   rol   rax, 28
   mov   [rcx-88], rax
   mov   rax, [rdx-56]
   xor   rax, r10
   rol   rax, 20
   mov   [rcx-80], rax
   mov   rax, [rdx-48]
   xor   rax, r11
   rol   rax, 3
   mov   [rcx-72], rax
   mov   rax, [rdx]
   xor   rax, r12
   rol   rax, 45
   mov   [rcx-64], rax
   mov   rax, [rdx+48]
   xor   rax, r13
   rol   rax, 61
   mov   [rcx-56], rax
   mov   rax, [rdx-120]
   xor   rax, r12
   rol   rax, 1
   mov   [rcx-48], rax
   mov   rax, [rdx-72]
   xor   rax, r13
   rol   rax, 6
   mov   [rcx-40], rax
   mov   rax, [rdx-24]
   xor   rax, r14
   rol   rax, 25
   mov   [rcx-32], rax
   mov   rax, [rdx+24]
   xor   rax, r10
   rol   rax, 8
   mov   [rcx-24], rax
   mov   rax, [rdx+32]
   xor   rax, r11
   rol   rax, 18
   mov   [rcx-16], rax
   mov   rax, [rdx-96]
   xor   rax, r10
   rol   rax, 27
   mov   [rcx-8], rax
   mov   rax, [rdx-88]
   xor   rax, r11
   rol   rax, 36
   mov   [rcx], rax
   mov   rax, [rdx-40]
   xor   rax, r12
   rol   rax, 10
   mov   [rcx+8], rax
   mov   rax, [rdx+8]
   xor   rax, r13
   rol   rax, 15
   mov   [rcx+16], rax
   mov   rax, [rdx+56]
   xor   rax, r14
   rol   rax, 56
   mov   [rcx+24], rax
   mov   rax, [rdx-112]
   xor   rax, r13
   rol   rax, 62
   mov   [rcx+32], rax
   mov   rax, [rdx-64]
   xor   rax, r14
   rol   rax, 55
   mov   [rcx+40], rax
   mov   rax, [rdx-16]
   xor   rax, r10
   rol   rax, 39
   mov   [rcx+48], rax
   mov   rax, [rdx-8]
   xor   rax, r11
   rol   rax, 41
   mov   [rcx+56], rax
   mov   rax, [rdx+40]
   xor   rax, r12
   rol   rax, 2
   mov   [rcx+64], rax
   // Chi
   mov   rax, [rcx-120]
   not   rax
   and   rax, [rcx-112]
   xor   rax, [rcx-128]
   mov   [rdx-128], rax
   mov   rax, [rcx-112]
   not   rax
   and   rax, [rcx-104]
   xor   rax, [rcx-120]
   mov   [rdx-120], rax
   mov   rax, [rcx-104]
   not   rax
   and   rax, [rcx-96]
   xor   rax, [rcx-112]
   mov   [rdx-112], rax
   mov   rax, [rcx-96]
   not   rax
   and   rax, [rcx-128]
   xor   rax, [rcx-104]
   mov   [rdx-104], rax
   mov   rax, [rcx-128]
   not   rax
   and   rax, [rcx-120]
   xor   rax, [rcx-96]
   mov   [rdx-96], rax
   mov   rax, [rcx-80]
   not   rax
   and   rax, [rcx-72]
   xor   rax, [rcx-88]
   mov   [rdx-88], rax
   mov   rax, [rcx-72]
   not   rax
   and   rax, [rcx-64]
   xor   rax, [rcx-80]
   mov   [rdx-80], rax
   mov   rax, [rcx-64]
   not   rax
   and   rax, [rcx-56]
   xor   rax, [rcx-72]
   mov   [rdx-72], rax
   mov   rax, [rcx-56]
   not   rax
   and   rax, [rcx-88]
   xor   rax, [rcx-64]
   mov   [rdx-64], rax
   mov   rax, [rcx-88]
   not   rax
   and   rax, [rcx-80]
   xor   rax, [rcx-56]
   mov   [rdx-56], rax
   mov   rax, [rcx-40]
   not   rax
   and   rax, [rcx-32]
   xor   rax, [rcx-48]
   mov   [rdx-48], rax
   mov   rax, [rcx-32]
   not   rax
   and   rax, [rcx-24]
   xor   rax, [rcx-40]
   mov   [rdx-40], rax
   mov   rax, [rcx-24]
   not   rax
   and   rax, [rcx-16]
   xor   rax, [rcx-32]
   mov   [rdx-32], rax
   mov   rax, [rcx-16]
   not   rax
   and   rax, [rcx-48]
   xor   rax, [rcx-24]
   mov   [rdx-24], rax
   mov   rax, [rcx-48]
   not   rax
   and   rax, [rcx-40]
   xor   rax, [rcx-16]
   mov   [rdx-16], rax
   mov   rax, [rcx]
   not   rax
   and   rax, [rcx+8]
   xor   rax, [rcx-8]
   mov   [rdx-8], rax
   mov   rax, [rcx+8]
   not   rax
   and   rax, [rcx+16]
   xor   rax, [rcx]
   mov   [rdx], rax
   mov   rax, [rcx+16]
   not   rax
   and   rax, [rcx+24]
   xor   rax, [rcx+8]
   mov   [rdx+8], rax
   mov   rax, [rcx+24]
   not   rax
   and   rax, [rcx-8]
   xor   rax, [rcx+16]
   mov   [rdx+16], rax
   mov   rax, [rcx-8]
   not   rax
   and   rax, [rcx]
   xor   rax, [rcx+24]
   mov   [rdx+24], rax
   mov   rax, [rcx+40]
   not   rax
   and   rax, [rcx+48]
   xor   rax, [rcx+32]
   mov   [rdx+32], rax
   mov   rax, [rcx+48]
   not   rax
   and   rax, [rcx+56]
   xor   rax, [rcx+40]
   mov   [rdx+40], rax
   mov   rax, [rcx+56]
   not   rax
   and   rax, [rcx+64]
   xor   rax, [rcx+48]
   mov   [rdx+48], rax
   mov   rax, [rcx+64]
   not   rax
   and   rax, [rcx+32]
   xor   rax, [rcx+56]
   mov   [rdx+56], rax
   mov   rax, [rcx+32]
   not   rax
   and   rax, [rcx+40]
   xor   rax, [rcx+64]
   mov   [rdx+64], rax
   pop  r14
   pop  r13
   pop  r12

