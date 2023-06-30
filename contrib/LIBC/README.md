# LIBC
a C Run Time library for Delphi, helps link with other C object files

Delphi come up a build in unit named System.Win.Crtl do the same job. How ever, this build in Unit didn't cover enough C functions.
I just copy paste this Unit, and Appends any functions I found when linking other C object files into it.
So, it is a better way to use this unit than the buildin one.

系统自带单元System.Win.Crtl的复制粘贴+扩展版本，覆盖更多的C标准库和一些特定编译器的内建函数，当你想链接的C目标文件调用到这些函数时，添加一句uses libc可能可以帮助你成功链接。
