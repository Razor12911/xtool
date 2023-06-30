# ZSTD static link library for DELPHI

I got a lot problem try linking zstd with delphi since 1.5.1, everything is smooth when it was 1.5.0
I got AV error when using mingw GCC 11.2, and failed to compile when using mingw clang 13.0.1
And finally I got it success compiled with VS 17.1.1, but delphi raise a internal AV error when linking with 32bit object files.
So, I compiled 32bit object file with VS Clang 13.0 tool set, seems it work, but it won't work on 64bit mode(AV in run time).
So, VC tool set only works on 64bit mode, and Clang only works on 32bit mode, I just combined two together...

Some thing you should know:
This library depends on other two librarys:

- [libc](https://github.com/YWtheGod/LIBC)
- [XXHASH4DELPHI](https://github.com/YWtheGod/XXHASH4Delphi)
- ZSTD on Github: [ZSTD](https://github.com/facebook/zstd)
- For Demos: [YW_DEMOS](https://github.com/YWtheGod/YW_DEMOS)

ZSTD comes with a xxhash source code with its own, but I thought sharing a library would be better.
And I didn't compiled legecy supporting code into the object files, so it can't decompress data that was compressed by very old zstd library(before 1.0 I thought), if it will be a problem, let me known.
Now I compiled both AVX2 and SSE2 version of the codes, but if to run it on CPU that don't have AVX2 support, you also need to compile my XXHASH4Delphi library in SSE2 mode too.
It didn't use ASM codes for huffman decompressing on Windows platform, I don't think decompressing is really a matter, so I just let it be.
And finally, the 1.5.2 version of zstd runs about same speeds with 1.5.0, slighly slower in high compressing rate setting. but it also got slighly better compression rates on all compression levels.

我在编译1.5.1以上版本时遇到很多问题, gcc编译出来的代码总是AV错, mingw上的Clang编译不过
最终用的是VC 17.1.1编译过了，64位初步测试正常, 32位下Delphi链接时报内部AV错了!
用VC切换到Clang工具集, 32位下初步测试过了，64位竟然继续报AV错!
没办法，最终提供的目标文件，64位是VC++工具集编译的, 32位是VS Clang 13.0工具集编译的。

这个库依赖另外两个库，需要另外下载安装：

-  [libc](https://github.com/YWtheGod/LIBC)
- [XXHASH4DELPHI](https://github.com/YWtheGod/XXHASH4Delphi)
- ZSTD官方Github: [ZSTD](https://github.com/facebook/zstd)
- 使用范例: [YW_DEMOS](https://gitee.com/YWtheGod/YW_DEMOS)

ZSTD自带一个xxhash源码，并且它修改了xxhash源码的命名前缀以独占这些代码，我取消了独占前缀，让它与其他模块共享独立安装的XXHASH库。
我并没有将ZSTD老算法兼容代码编译进去，所以它不能解压一些很老版本的ZSTD压缩数据(我估计起码老于1.0版本).
这次我附带了AVX2和SSE2版本的目标文件，通过编译开关可以切换，但是单独切换ZSTD还不够，你还得在编译XXHASH4DELPHI时也选择同样的SSE2版本，才能在没有AVX2的CPU上运行。
ZSTD在Windows平台下没有采用汇编代码的哈夫曼解压，我觉得解压也不需要极限速度，所以我就由得它没动它的汇编代码。
最后，1.5.2速度基本和1.5.0版本差不多，高压缩设定下轻微慢一点点，但是在所有压缩率设定下，压缩体积都要更小一点点
