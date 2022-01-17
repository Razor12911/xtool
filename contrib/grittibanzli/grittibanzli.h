// Copyright 2018 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef GRITTIBANZLI_H_
#define GRITTIBANZLI_H_

#include <stdint.h>
#include <string.h>

#include <vector>

namespace grittibanzli {

// Splits a deflate stream into the uncompressed data inside, and, choices data
// from which the original deflate stream (with same LZ77 lengths and so on)
// can be reconstructed. The predicted choices stream is large but has a lot
// of zeroes so it is very compressible.
// After use, compress both the encoded choices and the original data with a
// stronger compressor to achieve total filesize smaller than the original
// deflate file.
bool Grittibanzli(const uint8_t* deflated, size_t deflated_size,
                  std::vector<uint8_t>* uncompressed,
                  std::vector<uint8_t>* choices_encoded);

// Reconstructs the deflate stream from the uncompressed data and encoded
// choices. Appends to deflated if not empty.
bool Ungrittibanzli(const uint8_t* uncompressed, size_t uncompressed_size,
                    const uint8_t* choices_encoded, size_t choices_size,
                    std::vector<uint8_t>* deflated);

}  // namespace grittibanzli

#endif  // GRITTIBANZLI_H_
