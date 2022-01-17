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

// This is an example binary using grittibanzli, supporting pure deflate and
// gzip formats.

#include <limits.h>
#include <stdlib.h>

#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>
#include <memory>

#include "grittibanzli.h"

namespace grittibanzli {

namespace {
// Returns true iff the file exists and is readable
bool FileExists(const std::string& filename) {
  FILE* file = fopen(filename.c_str(), "rb");
  bool result = (file != 0);
  if (file) fclose(file);
  return result;
}

// Returns -1 on error, such as when file doesn't exist
int64_t GetFileSize(const std::string& filename) {
  FILE* file = fopen(filename.c_str(), "rb");
  if (!file) return -1;
  if (fseek(file, 0, SEEK_END) != 0) {
    fclose(file);
    return -1;
  }
  int64_t size = ftell(file);
  if (size == LONG_MAX) size = -1;
  fclose(file);
  return size;
}

bool LoadFile(const std::string& filename, std::vector<uint8_t>* result) {
  int64_t size = GetFileSize(filename);
  if (size < 0) return false;
  result->resize(size);
  if (size == 0) return true;

  FILE* file = fopen(filename.c_str(), "rb");
  if (!file) return false;
  size_t readsize = fread(result->data(), 1, size, file);
  fclose(file);

  if (readsize != static_cast<size_t>(size)) return false;

  return true;
}

bool SaveFile(const std::vector<uint8_t>& data, const std::string& filename) {
  FILE* file = fopen(filename.c_str(), "wb" );
  if (!file) return false;
  fwrite((char*)data.data() , 1 , data.size(), file);
  fclose(file);
  return true;
}

bool ParseGzipHeaders(const std::vector<uint8_t>& data,
                      int* deflate_begin, int* deflate_end) {
  if (data.size() < 10) return false;
  int flag = data[3];
  size_t pos = 10;
  if (flag & 2) {
    if (pos + 2 <= data.size()) return false;
    // skip FEXTRA
    pos += data[pos] + 256 * data[pos + 1];
  }
  if (flag & 4) {
    // skip FNAME
    while (pos < data.size() && data[pos]) pos++;
    pos++;
  }
  if (flag & 8) {
    // skip FCOMMENT
    while (pos < data.size() && data[pos]) pos++;
    pos++;
  }
  if (pos + 8 >= data.size()) return false;
  *deflate_begin = pos;
  *deflate_end = data.size() - 8;
  return true;
}
}  // namespace

void PrintHelp(const char* argv0) {
  std::cout << "Grittibanzli encodes all information needed to reconstruct "
               " a deflate compressed file (gz or pure deflate) into an easy"
               " to compress format\n"
            << "Usage: " << argv0
            << " [OPTIONS] [--input=FILENAME] [--output=FILENAME]"
               "  [--data=FILENAME]  [--choices=FILENAME]  [--meta=FILENAME]\n"
               "Options:\n"
               "  -d: decode instead of encode. decode = reconstruct gz file,"
               " encode = split gz file into data, compressible deflate choices"
               " and metadata\n"
               "  -f: force to overwrite when output files already exist\n"
               "  -h: show this help\n"
               "  -q: quiet: don't print stats when compressing\n"
               "  -v: verify after encoding or decoding\n"
               "  --pure_deflate: use pure deflate instead of gzip\n"
               "  --input: gz filename when encoding\n"
               "  --output: gz filename when decoding\n"
               "  --choices: encoded deflate choices filename.\n"
               "  --data: uncompressed data filename.\n"
               "  --meta: metadata filename (unused for pure_deflate).\n"
            << std::endl;
}

void InvalidArgument(const char* argv0, const std::string& message = "") {
  std::cout << "Invalid Arguments" << message << std::endl;
  PrintHelp(argv0);
  std::exit(1);
}

void DieWithError(const std::string& text) {
  std::cout << text << std::endl;
  std::exit(1);
}

#define TryWithError(code, errortext) if (!(code)) DieWithError(errortext);

bool Encode(bool format_gzip, const std::vector<uint8_t>& in,
    std::vector<uint8_t>* data, std::vector<uint8_t>* choices,
    std::vector<uint8_t>* meta) {
  int start = 0, end = in.size();
  if (format_gzip) {
    TryWithError(ParseGzipHeaders(in, &start, &end), "Invalid gzip file");
    meta->assign(in.begin(), in.begin() + start);
    meta->insert(meta->end(), in.begin() + end, in.end());
  }

  const uint8_t* deflate = in.data() + start;
  size_t deflate_size = end - start;
  return Grittibanzli(deflate, deflate_size, data, choices);
}

bool Decode(bool format_gzip, const std::vector<uint8_t>& data,
    const std::vector<uint8_t>& choices, const std::vector<uint8_t>& meta,
    std::vector<uint8_t>* out) {
  if (format_gzip) {
    if (meta.size() < 8) DieWithError("Invalid gzip metadata");
    out->assign(meta.begin(), meta.end() - 8);
  }

  if (!Ungrittibanzli(data.data(), data.size(),
      choices.data(), choices.size(), out)) return false;

  if (format_gzip) {
    out->insert(out->end(), meta.end() - 8, meta.end());
  }

  return true;
}

int Run(int argc, char *argv[]) {
  bool decode = false;
  bool force = false;
  bool verbose = true;
  bool verify = false;
  bool format_gzip = true;
  std::string fname_input;
  std::string fname_output;
  std::string fname_choices;
  std::string fname_data;
  std::string fname_meta;
  for (int i = 1; i < argc; i++) {
    std::string arg(argv[i]);
    if (arg.size() < 2) InvalidArgument(argv[0]);

    if (arg == "--pure_deflate") {
      format_gzip = false;
    } else if (arg[0] == '-' && arg[1] != '-') {
      for (size_t j = 1; j < arg.size(); j++) {
        if (arg[j] == 'd') {
          decode = true;
        } else if (arg[j] == 'f') {
          force = true;
        } else if (arg[j] == 'q') {
          verbose = false;
        } else if (arg[j] == 'v') {
          verify = true;
        } else if (arg[j] == 'h') {
          PrintHelp(argv[0]);
          return 0;
        } else {
          std::string unknown = " ";
          unknown[0] = arg[j];
          InvalidArgument(argv[0], ": unknown option " + unknown);
        }
      }
    } else {
      size_t eq = arg.find('=');
      std::string key, value;
      if (eq == std::string::npos) {
        key = arg;
        if (i + 1 >= argc) InvalidArgument(argv[0]);
        value = argv[i + 1];
        i++;
      } else {
        key = arg.substr(0, eq);
        value = arg.substr(eq + 1);
      }
      if (value.empty()) InvalidArgument(argv[0]);
      if (key == "--input") {
        if (value.empty()) InvalidArgument(argv[0]);
        fname_input = value;
      } else if (key == "--output") {
        if (value.empty()) InvalidArgument(argv[0]);
        fname_output = value;
      } else if (key == "--choices") {
        if (value.empty()) InvalidArgument(argv[0]);
        fname_choices = value;
      } else if (key == "--data") {
        if (value.empty()) InvalidArgument(argv[0]);
        fname_data = value;
      } else if (key == "--meta") {
        if (value.empty()) InvalidArgument(argv[0]);
        fname_meta = value;
      } else {
        InvalidArgument(argv[0], ": unknown key " + key);
      }
    }
  }


  if (decode) {
    if (fname_output.empty()) InvalidArgument(argv[0], ": --output missing");
    if (!fname_input.empty()) {
      InvalidArgument(argv[0], ": --input may not be given when decoding,"
                               " use --choices, --data and --meta as inputs");
    }
    if (fname_choices.empty()) InvalidArgument(argv[0], ": --choices missing");
    if (fname_data.empty()) InvalidArgument(argv[0], ": --data missing");
    if (!format_gzip && fname_meta.empty()) {
      InvalidArgument(argv[0], ": --meta missing");
    }

    if (!force && FileExists(fname_output)) {
      DieWithError(fname_output + " already exists");
    }

    std::vector<uint8_t> data, choices, meta, out;
    TryWithError(LoadFile(fname_choices, &choices),
        "Could not load " + fname_choices);
    TryWithError(LoadFile(fname_data, &data), "Could not load " + fname_data);
    if (format_gzip) {
      TryWithError(LoadFile(fname_meta, &meta), "Could not load " + fname_meta);
    }

    TryWithError(Decode(format_gzip, data, choices, meta, &out),
                 "Failed to decode to " + fname_output);

    if (verify) {
      std::vector<uint8_t> data2, choices2, meta2;
      if (!Encode(format_gzip, out, &data2, &choices2, &meta2)
          || data2 != data) {
        DieWithError("Failed to verify " + fname_output);
      }
    }

    TryWithError(SaveFile(out, fname_output),
                 "Failed to save " + fname_output);
  } else {
    if (fname_input.empty()) InvalidArgument(argv[0], ": --input missing");
    if (!fname_output.empty()) {
      InvalidArgument(argv[0], ": --output may not be given when encoding,"
                               " use --choices, --data and --meta as inputs");
    }
    if (!force && FileExists(fname_choices)) {
      DieWithError(fname_choices + " already exists");
    }
    if (!force && FileExists(fname_data)) {
      DieWithError(fname_data + " already exists");
    }
    if (!force && FileExists(fname_meta)) {
      DieWithError(fname_meta + " already exists");
    }

    std::vector<uint8_t> in, data, choices, meta;
    if (!LoadFile(fname_input, &in)) {
      DieWithError("Could not load " + fname_input);
    }
    TryWithError(Encode(format_gzip, in, &data, &choices, &meta),
                 "Failed to encode " + fname_input);

    if (verify) {
      std::vector<uint8_t> in2;
      if (!Decode(format_gzip, data, choices, meta, &in2) || in2 != in) {
        DieWithError("Failed to verify " + fname_input);
      }
    }

    if (!fname_data.empty()) {
      TryWithError(SaveFile(data, fname_data), "Failed to save " + fname_data);
    }
    if (!fname_choices.empty()) {
      TryWithError(SaveFile(choices, fname_choices),
                   "Failed to save " + fname_choices);
    }
    if (format_gzip && !fname_meta.empty()) {
      TryWithError(SaveFile(meta, fname_meta), "Failed to save " + fname_meta);
    }

    if (verbose) {
      int zeroes = 0;
      for (size_t i = 0; i < choices.size(); i++) {
        if (!choices[i]) zeroes++;
      }
      std::cout << "input size: " << in.size()
                << ", uncompressed size: " << data.size()
                << ", metadata size: " << meta.size()
                << ", choices size: " << choices.size()
                << " of which " << (choices.size() - zeroes) << " non-zeroes"
                << std::endl;
    }
  }

  return 0;
}
}  // namespace grittibanzli

int main(int argc, char *argv[]) {
  return grittibanzli::Run(argc, argv);
}
