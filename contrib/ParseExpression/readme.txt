----- TExpressionParser -----

A flexible and fast expression parser object for logical and 
mathematical functions.
Author: Egbert van Nes
Status: Freeware with source
Version: 1.2
Delphi version: D4 and D5 (probably usable for D2 and D3 too)
Date: April 2002

Download Delphi 4/5 source code (17K)


Desciption
The fast evaluation algorithm ('pseudo-compiler' generating a linked 
list that evaluates fast) is based upon TParser - an extremely fast 
component for parsing and evaluating mathematical expressions ('pseudo-
compiled' code is only 40-80% slower than compiled Delphi code).

See also: http://www.datalog.ro/delphi/parser.html (by Renate Schaaf,
1993; Alin Flaider, 1996; Version 9-10: Stefan Hoffmeister, 1996-1997)

I used that valuable free parser for some years but needed to 
add logical operands, which was more difficult for me than rewriting 
the parser.

TExpressionParser is approximately equally fast in evaluating 
expressions as TParser, but the compiling is made object oriented, 
and programmed recursively, requiring much less code and making it 
easier to customize the parser.
From version 1.1 on optimization is added, making repeated evaluation 
often even faster.
Furthermore, there are several operands added: 
  comparison: > < <> = <= >= (work also on strings) 
  logical: and or xor not 
  factorial: ! 
  percentage: % 
  assign to variables: := 

User defined functions can have maximal maxArg (=4) parameters 
set MaxArg (in unit ParseClass) to a higher value if needed.

The required format of the expression is Pascal style (optionally
C++ style operands are also supported) with the following additional
operands: 
  factorial (x!) 
  power (x^y) 
  percentage (x%)

Implicit multiplying is not supported: e.g. (X+1)(24-3) generates 
a syntax error and should be replaced by (x+1)*(24-3)

Logical functions evaluate in 0 if False and 1 if True The AsString 
property returns True/False if the expression is logical.

The comparison functions (> <> < etc.) work also with string constants
('string') and string variables. These comparisons are not case 
sensitive.

The precedence of the operands is little different from Pascal 
(Delphi), giving a lower precedence to logical operands, as these 
by default only act on Booleans (and not on integers like in Pascal). 
This behavior is easily adjustable. 
  (highest): ! -x +x % 
  ^ 
  * / div mod 
  + - 
  > >= < <= <> = 
  not 
  or and xor 
  (lowest): := 
This precedence order is easily customizable by overriding/changing 
the FillExpressList method (the precedence order is defined there).

You can use user-defined variables in the expressions and also assign 
to variables using the := operand

The use of this object is very simple, therefore it doesn't seem 
necessary to make a non-visual component of it.

NEW IN VERSION 1.1: 
Optimization, increasing the efficiency for evaluating an expression 
many times (with a variable in the expression). The 'compiler' then
removes constant expressions and replaces these with the evaluated
result.
e.g. 4*4*x becomes 16*x
     ln(5)+3*x becomes 1.609437912+3*x

limitation:
     4*x+3+3+5 evaluates as 4*x+3+3+5 (due to precedence rules)
whereas:
     4*x+(3+3+5) becomes 4*x+11 (use brackets to be sure that constant 
     expressions are removed by the compiler)




New in Version 1.1.1
- Evaluation of hexadecimal numbers (e.g. $FF, the $-sign for hexadecimals is adjustable) and show result as hex. 
- Changes in class implementation 



New in Version 1.1.2
- The variable DecimalSeparator (SysUtils) now determines the decimal separator. If the decimal separator is a comma
  then the function argument separator is a semicolon ';' 


New in Version 1.1.3

- Rearranged the classes, added a basic class TCustomParser for maximal flexibility. 
- Multiline formula parser (a contribution by Xavier Mor-Mur, xmormur@telepolis.com, xmormur@teleline.es) 
- New example application for multiline formula's 
- ++ and -- support (like C++)

Contact with the author
author: Egbert van Nes
email: Egbert.vanNes@aqec.wkao.wau.nl

You are encouraged to send bug reports. 
