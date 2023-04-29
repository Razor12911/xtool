# XXHASH4Delphi
XXHash Wrapper for Delphi
prebuild XXHash 0.8.1 Static Linked Object file with AVX2 or SSE2 support for both X64 and X86 platform.
Simple test program provided.
 
xxHash is an Extremely fast Hash algorithm, running at RAM speed limits. It successfully completes the SMHasher test suite which evaluates collision, dispersion and randomness qualities of hash functions. Code is highly portable, and hashes are identical across all platforms (little / big endian).

precompiled object files were compiled with GCC 11.2 with -O3 and -mAVX2 or -mSSE2

Check [XXHASH](https://github.com/Cyan4973/xxHash) for details.
For demos, check [YW_DEMOS](https://github.com/YWtheGod/YW_DEMOS)

XXHash 0.8.1的静态链接库，支持所有平台，在非Windows平台上直接链接系统提供的libxxhash.a静态库文件，如需特殊指令集优化请自行重新编译libxxhash.a文件。

Windows平台下默认采用avx2指令集，如需兼容老电脑，可删除XXHASHLIB.pas文件中的第二行{$DEFINE AVX2}，即可选择链接SSE2指令集的目标文件。
静态链接的目标文件用GCC 11.2版本-O3优化编译并通过-m参数指定指令集。

官方github: [XXHASH](https://github.com/Cyan4973/xxHash)
使用例子：[YW_DEMOS](https://gitee.com/YWtheGod/YW_DEMOS)

此算法惊人地快，适合作为MD5替代算法使用。
