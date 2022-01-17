{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory
  of this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}

//------------------------------------------------------------------------------
// Reset Defines (do not change!)
//------------------------------------------------------------------------------

{$UNDEF NATIVEINT_UNDEFINED}

{$UNDEF PUREPASCAL}
{$UNDEF X86ASM}
{$UNDEF X64ASM}
{$UNDEF NO_ASM}

{$UNDEF OLD_SHA_NAME}
{$UNDEF OLD_WHIRLPOOL_NAMES}
{$UNDEF OLD_REGISTER_FAULTY_CIPHERS}
{$UNDEF AUTO_PRNG}
{$UNDEF DEC52_IDENTITY}
{$UNDEF DEC3_CMCTS}

{$UNDEF RESTORE_RANGECHECKS}
{$UNDEF RESTORE_OVERFLOWCHECKS}

//------------------------------------------------------------------------------
// User Configuration (feel free to edit)
//------------------------------------------------------------------------------

// If on, the old class name THash_SHA for the THash_SHA0 class will be available
// This should only be necessary for cases where the identity value has been
// used to determine the algorithm to be used and thus it's necessary that the
// same result will be produced. It is recommended to switch to the new class
// names where possible
// Currently this does not work together with using x86 ASM! Either ASM or the
// old name works
{.$DEFINE OLD_SHA_NAME} (* default off *)

// if on the old outdated class names THash_Whirlpool and THash_Whirlpool1 are
// being used. Nowadays Whirlpool versions are named like this:
// Whirlpool -> Whirlpool0. Whrilpool1 -> WhirlpoolT and there's a new Whirlpool1
// variant which differs to WhrilpoolT only by the initialization of internal data
// This should only be necessary for cases where the identity value has been
// used to determine the algorithm to be used and thus it's necessary that the
// same result will be produced. It is recommended to switch to the new class
// names where possible
{.$DEFINE OLD_WHIRLPOOL_NAMES} (* default off *)

// when enabling this define, the automatic registration of all those cipher and
// hash classes in initialization sections is not being done.
{.$DEFINE ManualRegisterClasses} (* default off *}

// Automatically use DEC's Pseudo Random Number Generator
{$DEFINE AUTO_PRNG} (* default ON *)

// if the compiler does not support assembler turn usage off and even if restrict
// it to Windows, as those non Windows platforms which actually do support ASM
// in Delphi do not use Intel x86 ASM
{$IFNDEF FPC}
  {$IFNDEF ASSEMBLER}
    {$DEFINE NO_ASM} (* default ON *)
  {$ELSE}
    {$IFDEF WINDOWS}
      {.$DEFINE NO_ASM} (* default OFF *)
    {$ELSE
      {$DEFINE NO_ASM} (* default ON *)
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  // Turn ASM off for FPC as we do not know enough about ASM support on FPC
  {$DEFINE NO_ASM} (* default ON *)
{$ENDIF}

// Enable the following option to restore the *wrong* Identity behavior of
// DEC version 5.2. See TDECObject.Identity in DECBaseClass.pas for more details
{.$DEFINE DEC52_IDENTITY} (* default OFF *)

// Enable the following option to include the cmCTS3 block cipher mode in
// DECCipher. This mode is less secure due to the padding used on data smaller
// that cipher's block size but it might help when having to deal with
// data which needs to be compatible to the DEC 3.0 cmCTS mode. Not recommended!
{.$DEFINE DEC3_CMCTS} (* default OFF *)

/// <summary>
///   When building a Firemonkey project for non Windows platforms
///   ressource strings are not available. Enabling this enables some
///   alternative translation mechanism for exception messages based
///   on Firemonkey's TLang architecture
/// </summary>
{$IF DECLARED(FireMonkeyVersion)}
  {$DEFINE FMXTranslateableExceptions}
{$IFEND} { TODO: convert to $ENDIF when raising minimum supported version to XE4+}
//------------------------------------------------------------------------------
// Do NOT change anything below!
//------------------------------------------------------------------------------

{$DEFINE DELPHIORBCB}

//------------------------------------------------------------------------------
// FPC v2.x support (Experimental)
//------------------------------------------------------------------------------

{$IFDEF FPC}
  {$UNDEF DELPHIORBCB}

  {$DEFINE PUREPASCAL}

  {$DEFINE NATIVEINT_UNDEFINED}

  // use compatibility mode
  {$MODE DELPHI}

  // defines for Mac OS X
  {$IFDEF DARWIN}
    {$DEFINE MACOS}
    {$DEFINE ALIGN_STACK}
  {$ENDIF}
{$ENDIF FPC}

//------------------------------------------------------------------------------
// Architecture (x86ASM, x64ASM, PurePascal)
//------------------------------------------------------------------------------

//{$DEFINE NO_ASM}

{$IFDEF NO_ASM}
  // User config override (see top of this file)
  {$UNDEF X86ASM}
  {$UNDEF X64ASM}
  {$DEFINE PUREPASCAL}
{$ENDIF}

{$IFNDEF PUREPASCAL}
  // ignored by FPC (already in PurePascal mode as defined in FPC block above)
  {$IFDEF CPUX86}
    {$DEFINE X86ASM}
  {$ELSE !CPUX86}
    {$IFDEF CPUX64}
      {$DEFINE X64ASM}
    {$ELSE !CPUX64}
      {$DEFINE PUREPASCAL}
    {$ENDIF !CPUX64}
  {$ENDIF}
{$ENDIF !PUREPASCAL}

{$IF SizeOf(Pointer) = 4}
  {$DEFINE CPU32BITS}
{$IFEND}
{$IF SizeOf(Pointer) = 8}
  {$DEFINE CPU64BITS}
{$IFEND}

//------------------------------------------------------------------------------
// Delphi and C++ Builder
//------------------------------------------------------------------------------

{$IFDEF DELPHIORBCB}
  {$BOOLEVAL OFF} // short-circuit bool eval (Default OFF; Local Scope)

  {$IF CompilerVersion >= 20} // Delphi 2009 and newer
    {$IF CompilerVersion >= 21}   // Delphi 2010 and newer
      {$DEFINE DELPHI_2010_UP}
    {$IFEND}
  {$ELSE}
    Sorry, but Delphi 2007 and lower are no longer supported!
  {$IFEND}
{$ENDIF}

//------------------------------------------------------------------------------
// Asm Core
//------------------------------------------------------------------------------

{$IFDEF X86ASM}
  {$DEFINE THash_MD4_asm}
  {$DEFINE THash_MD5_asm}
  {$DEFINE THash_RipeMD128_asm}
  {$DEFINE THash_RipeMD160_asm}
  {$DEFINE THash_RipeMD256_asm}
  {$DEFINE THash_RipeMD320_asm}
  {$DEFINE THash_SHA_asm}
  {$DEFINE THash_SHA256_asm}
  {$DEFINE THashBaseHaval_asm}
  {$DEFINE THash_Panama_asm}
  {$DEFINE THashBaseWhirlpool_asm}
  {$DEFINE THash_Sapphire_asm}

  // The following asm optimized Hashes were incompatible with newer C++
  // Builder versions. Due to the fact that BCB detection is not implemented
  // in newer Delphi versions anymore, we leave them out for now:

  // {$DEFINE THash_MD2_asm}
  // {$DEFINE THash_SHA384_asm}
  // {$DEFINE THash_Tiger_asm}
  // {$DEFINE THash_Square_asm}
  // {$DEFINE THash_Snefru128_asm}
  // {$DEFINE THash_Snefru256_asm}
{$ENDIF}
