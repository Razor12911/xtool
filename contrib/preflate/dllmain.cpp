// dllmain.cpp : Defines the entry point for the DLL application.
#include "pch.h"
#include <algorithm>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <string.h>
#include <cstdint>
#include <iterator>
#include <vector>

#include "preflate_decoder.h"
#include "preflate_reencoder.h"

BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
                     )
{
    switch (ul_reason_for_call)
    {
    case DLL_PROCESS_ATTACH:
    case DLL_THREAD_ATTACH: 
    case DLL_THREAD_DETACH:
    case DLL_PROCESS_DETACH:
        break;
    }
    return TRUE;
}

extern "C" __declspec(dllexport) bool decode(const unsigned char* src,
	int srcSize, unsigned char* dst1, int* dst1Capacity, unsigned char* dst2,
	int* dst2Capacity) {
	std::vector<unsigned char>deflate_raw(srcSize);
	std::vector<unsigned char>unpacked_output;
	std::vector<unsigned char>preflate_diff;
	memcpy(deflate_raw.data(), src, srcSize);
	if ((preflate_decode(unpacked_output, preflate_diff, deflate_raw, *dst2Capacity) == true) && (unpacked_output.size() <= *dst1Capacity)
		&& (preflate_diff.size() <= *dst2Capacity)) {
		*dst1Capacity = unpacked_output.size();
		memcpy(dst1, unpacked_output.data(), unpacked_output.size());
		*dst2Capacity = preflate_diff.size();
		memcpy(dst2, preflate_diff.data(), preflate_diff.size());
		return true;
	}
	else {
		return false;
	}
}

extern "C" __declspec(dllexport) bool reencode(const unsigned char* src1,
	int src1Size, const unsigned char* src2,
	int src2Size, unsigned char* dst, int* dstCapacity) {
	std::vector<unsigned char>unpacked_input(src1Size);
	std::vector<unsigned char>preflate_diff(src2Size);
	std::vector<unsigned char>deflate_raw;
	memcpy(unpacked_input.data(), src1, src1Size);
	memcpy(preflate_diff.data(), src2, src2Size);
	if ((preflate_reencode(deflate_raw, preflate_diff, unpacked_input) == true) && (deflate_raw.size() <= *dstCapacity)) {
		*dstCapacity = deflate_raw.size();
		memcpy(dst, deflate_raw.data(), deflate_raw.size());
		return true;
	}
	else {
		return false;
	}
}


