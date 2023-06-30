#define ZSTD_NO_TRACE
#define XXH_NAMESPACE
#include"common/error_private.c"
#include"common/zstd_common.c"
#include"compress/zstd_compress.c"
#include"compress/zstd_ldm.c"
#include"compress/zstd_double_fast.c"
#include"compress/zstd_fast.c"
#include"compress/zstd_opt.c"
#include"compress/zstd_lazy.c"
#include"compress/zstd_compress_literals.c"
#include"compress/hist.c"
#include"compress/zstd_compress_sequences.c"
#include"compress/zstd_compress_superblock.c"
#include"compress/huf_compress.c"
#define FSE_isError entropy_FSE_isErr
#define HUF_isError entropy_HUF_isErr
#include"common/entropy_common.c"
#include"compress/fse_compress.c"
#include"common/fse_decompress.c"
#include"decompress/zstd_decompress.c"
#include"decompress/zstd_ddict.c"
#include"decompress/zstd_decompress_block.c"
#include"decompress/huf_decompress.c"
