// dllmain.cpp : Defines the entry point for the DLL application.
#include "pch.h"
#include "..\xdelta3.h"
#include "..\xdelta3.c"

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


extern "C" __declspec(dllexport) int xd3_encode(const uint8_t * input,
    usize_t        input_size,
    const uint8_t * source,
    usize_t        source_size,
    uint8_t * output_buffer,
    usize_t * output_size,
    usize_t   avail_output,
    int            flags) {
    return xd3_encode_memory(input, input_size, source, source_size, output_buffer, output_size, avail_output, flags);
}


extern "C" __declspec(dllexport) int xd3_decode(const uint8_t * input,
    usize_t        input_size,
    const uint8_t * source,
    usize_t        source_size,
    uint8_t * output_buf,
    usize_t * output_size,
    usize_t        avail_output,
    int            flags) {
    return xd3_decode_memory(input, input_size, source, source_size, output_buf, output_size, avail_output, flags);
}
