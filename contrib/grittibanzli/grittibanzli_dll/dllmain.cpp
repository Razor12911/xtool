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
#include "..\grittibanzli.h"

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
namespace grittibanzli {

	extern "C" __declspec(dllexport) bool __Grittibanzli(const uint8_t* src,
		size_t srcSize, uint8_t* dst1, size_t* dst1Capacity, uint8_t* dst2,
		size_t* dst2Capacity) {
		std::vector<uint8_t>uncompressed;
		std::vector<uint8_t>choices_encoded;
		if ((Grittibanzli(src, srcSize, &uncompressed, &choices_encoded) == true) && (uncompressed.size() <= *dst1Capacity)
			&& (choices_encoded.size() <= *dst2Capacity)) {
			*dst1Capacity = uncompressed.size();
			memcpy(dst1, uncompressed.data(), uncompressed.size());
			*dst2Capacity = choices_encoded.size();
			memcpy(dst2, choices_encoded.data(), choices_encoded.size());
			return true;
		}
		else {
			return false;
		}
	}

	extern "C" __declspec(dllexport) bool __Ungrittibanzli(const uint8_t* src1,
		size_t src1Size, const unsigned char* src2,
		size_t src2Size, unsigned char* dst, size_t* dstCapacity) {
		std::vector<uint8_t>deflated;
		if ((Ungrittibanzli(src1, src1Size, src2, src2Size, &deflated) == true) && (deflated.size() <= *dstCapacity)) {
			*dstCapacity = deflated.size();
			memcpy(dst, deflated.data(), deflated.size());
			return true;
		}
		else {
			return false;
		}
	}
}




