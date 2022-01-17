# DEC - Delphi Encryption Compendium

## What is DEC?
DEC is a library for Embarcadero Delphi, containing different cryptographic algorithms .
It contains algorithms for these categories:

* Ciphers: encryption/decryption of data
* Hashes: "cryptographic checksums"
* Key derivation algorithms like Kdf1-Kdf3 and pbkdf2
* HMAC message authentication
* A cryptographic pseudo random number generator
* CRCs: non cryptographic checksums based on CRC algorithms

## Which Delphi versions are compatible?
The current version 6.2 is compatible with Delphi 2009 - Delphi 10.4.2 Sydney. 
When defining the NO_ASM define in DECOptions.inc it is compatible with all 
platforms supported by Delphi! It might be compatible with FPC, but this has 
not been focus and is not tested. The development branch contains a more
FPC compatible version already.

If you need support for older Delphi versions use version 5.2, which is compatible 
with Delphi 7-2007 at least but lacks some hash implementations, HMAC and KDF 
improvements. While V5.2 can be made compatible with newer Delphi versions with
small modifications we strongly recommend to better adapt your code to use the
current version of DEC, given all these improvements made since then.
A list of changes is available in the docs folder.

## What is the current status of this project?
V6.0 was released shortly before Christmas 2020. Since then work continued
by some users supplying code, reporting bugs (regressions) along with fixes
and by adding SHA2-224 which was still missing.
Details about the changes and additions in V6.2 can be found in the 
VersionHistory.pdf file in the docs subfolder of the development branch.

In comparison to 5.2 we added some console, VCL and FMX based demo applications.
The FMX based demos are even available via Google play as "DEC cipher demo" and
"DEC hash demo".

## Where can I get further information? For example if I'd like to contribute?
In the root folder of DEC V6.2 you will find further files with information about 
this project like *NOTICE.txt*, *CONTRIBUTING.md*, *SECURITY.md*.
Also take the time to read DEC61.pdf in the *Docs* folder or look at the demos 
provided in the *Demos* subfolder.

## Has it been tested?
DEC 5.2 came with some "arcane" test program testing the algoithms implemented
using test data supplied via some text file. For many algorithms this test data
stems from official documentation of the algorithms itsself. DEC 5.2 passes these 
tests.

DEC 6.0 reworked these tests into DUnit and DUnitX tests. We also added some more 
tests and with this replaced the "arcane" test program which used hard to understand 
code. A few of the implemented unit tests may still fail, but this is simply because
they are empty sceletons at this point in time waiting to be filled in. We first 
need to work out how to implement these tests and maybe look for test data.
Why don't you help out by researching useful test data for those few tests?
We're talking at block chaining mode tests for the ciphers specifically.

In DEC 6.2 the unit tests for the hash classes were looked at and where not already 
used original test data vectors (as far as we could find them - for most we could) 
have been added to improve test coverage.

## Contained hash algorithms
* MD2        
* MD4        
* MD5        
* RipeMD128  
* RipeMD160  
* RipeMD256  
* RipeMD320  
* SHA0       
* SHA1    
* SHA224   
* SHA256     
* SHA384     
* SHA512     
* SHA3_224
* SHA3_256
* SHA3_384
* SHA3_512
* Haval128   
* Haval160   
* Haval192   
* Haval224   
* Haval256   
* Tiger      
* Panama     
* Whirlpool0 
* Whirlpool1 
* WhirlpoolT 
* Square     
* Snefru128  
* Snefru256  
* Sapphire   

## Contained cipher algorithms
* Null
* Blowfish
* Twofish
* IDEA
* Cast256
* Mars
* RC4
* RC6
* AES
* Square
* SCOP
* Sapphire
* 1DES
* 2DES
* 3DES
* 2DDES
* 3DDES
* 3TDES
* 3Way
* Cast128
* Gost
* Magma
* Misty
* NewDES
* Q128
* RC2
* RC5
* SAFER
* Shark
* Skipjack
* TEA
* XTEA
* TEAN

## Contained block concatenating modes
Modes ending on x have been invented by the original developer of DEC
* ECBx
* CBCx
* CTSx
* CTS3
* CFB8
* CFBx
* OFB8
* OFBx
* CFS8
* CFSx

## Contained key derivation algorithms:
* KDF1
* KDF2
* KDF3
* MGF1
* PBKDF2

## Contained message authentication algorithms
* HMAC

## Contained formattings
* Copy
* HEX      
* HEXL     
* Base16   
* Base16L  
* DECMIME32
* Base64   
* MIME64   
* Radix64  
* PGP      
* UU       
* XX       
* ESCAPE   

## Contained CRCs
* 8
* 10
* 12
* 16
* 16CCITT
* 16XModem
* 24
* 32
* 32CCITT
* 32ZModem
* 8ATMHEC
* 8SMBus
* 15CAN
* 16ZMODEM