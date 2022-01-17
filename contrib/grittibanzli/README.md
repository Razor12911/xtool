Grittibanzli is a tool to compress a deflate stream to a smaller file, which can
be decoded to the original deflate stream again. That is, it compresses not only
the data inside the deflate stream, but also the deflate-related information
such as LZ77 symbols and Huffman trees, to reproduce a gzip, png, ... file
exactly.

Usually compressing a compressed file does not work well, and Grittibanzli aims
to improve this situation for compressing deflate files.

Grittibanzli splits a deflate container into 2 or 3 streams:
-uncompressed data: the original data
-deflate choices needed to reproduce the exact deflate stream. It uses
prediction to encode it well no matter what deflate compressor (gzip -1 to -9,
zopfli, ...) was used. The prediction works best for gzip -6 to -9.
-when applicable: container metadata (such as gzip header)

These different byte streams then need to be compressed to produce a new file
smaller than the original deflate container. Suggested compression algorithms:

-uncompressed data: Brotli quality 11
-deflate choices: PPMd, or Brotli quality 10 or 11
-metadata: Brotli quality 11

Well compressed choices will be roughly 0.5-1% for gzip -6 to -9, 9% for zopfli,
21% for gzip -1. As long as this percentage is smaller than the gain from
Brotli-compressing the rest, the deflate container can be losslessly reduced.

Performing the compression is not included in this API. Grittibanzli's main
result is the deflate choices bytestream, which is large but has a lot of
zeroes so that you can compress it well.

This is not an official Google product.
