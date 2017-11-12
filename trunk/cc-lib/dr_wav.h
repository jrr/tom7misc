// WAV audio loader. Public domain. See "unlicense" statement at the end of
// this file.
// dr_wav - v0.5f - 2017-04-04
// David Reid - mackron@gmail.com
//
// Modified by Tom 7 in 2017:
//  - Make the documentation fit in 80 columns.
//  - Separate .h and .cc
//  - Improve usability from C++.

// Do something like the following to read audio data:
//
//   drwav wav;
//   if (!drwav_init_file(&wav, "my_song.wav")) {
//     // Error opening WAV file.
//   }
//
//   dr_int32* pDecodedInterleavedSamples =
//     malloc(wav.totalSampleCount * sizeof(dr_int32));
//   size_t numberOfSamplesActuallyDecoded =
//     drwav_read_s32(&wav, wav.totalSampleCount, pDecodedInterleavedSamples);
//
//   ...
//
//   drwav_uninit(&wav);
//
// You can also use drwav_open() to allocate and initialize the loader for you:
//
//   drwav* pWav = drwav_open_file("my_song.wav");
//   if (pWav == NULL) {
//     // Error opening WAV file.
//   }
//
//   ...
//
//   drwav_close(pWav);
//
// If you just want to quickly open and read the audio data in a
// single operation you can do something like this:
//
//   unsigned int channels;
//   unsigned int sampleRate;
//   dr_uint64 totalSamples;
//   float* pSampleData =
//       drwav_open_and_read_file_f32("my_song.wav",
//                                    &channels, &sampleRate, &totalSamples);
//   if (pSampleData == NULL) {
//     // Error opening and reading WAV file.
//   }
//
//   ...
//
//   drwav_free(pSampleData);
//
// The examples above use versions of the API that convert the audio
// data to a consistent format (32-bit signed PCM, in this case), but
// you can still output the audio data in it's internal format (see
// notes below for supported formats):
//
//   size_t samplesRead = drwav_read(&wav, wav.totalSampleCount,
//                                   pDecodedInterleavedSamples);
//
// You can also read the raw bytes of audio data, which could be
// useful if dr_wav does not have native support for a particular data
// format:
//
//   size_t bytesRead = drwav_read_raw(&wav, bytesToRead, pRawDataBuffer);
//
//
// dr_wav has seamless support the Sony Wave64 format. The decoder
// will automatically detect it and it should Just Work without any
// manual intervention.
//
//
//
// OPTIONS
// #define these options before including this file.
//
// #define DR_WAV_NO_CONVERSION_API
//   Disables conversion APIs such as drwav_read_f32() and drwav_s16_to_f32().
//
// #define DR_WAV_NO_STDIO
//   Disables drwav_open_file().
//
//
// QUICK NOTES
// - Samples are always interleaved.
// - The default read function does not do any data conversion. Use
//   drwav_read_f32() to read and convert audio data to IEEE 32-bit
//   floating point samples. Likewise, use drwav_read_s32() to read
//   and convert auto data to signed 32-bit PCM. Tested and supported
//   internal formats include the following:
//   - Unsigned 8-bit PCM
//   - Signed 12-bit PCM
//   - Signed 16-bit PCM
//   - Signed 24-bit PCM
//   - Signed 32-bit PCM
//   - IEEE 32-bit floating point.
//   - IEEE 64-bit floating point.
//   - A-law and u-law
// - Microsoft ADPCM is not currently supported.
// - dr_wav will try to read the WAV file as best it can, even if it's
//   not strictly conformant to the WAV format.


#ifndef __DR_WAV_H
#define __DR_WAV_H

#include <stddef.h>

// XXX: Just replace these with cstdint types -tom
#ifndef DR_SIZED_TYPES_DEFINED
#define DR_SIZED_TYPES_DEFINED
#if defined(_MSC_VER) && _MSC_VER < 1600
typedef   signed char    dr_int8;
typedef unsigned char    dr_uint8;
typedef   signed short   dr_int16;
typedef unsigned short   dr_uint16;
typedef   signed int     dr_int32;
typedef unsigned int     dr_uint32;
typedef   signed __int64 dr_int64;
typedef unsigned __int64 dr_uint64;
#else
#include <stdint.h>
typedef int8_t           dr_int8;
typedef uint8_t          dr_uint8;
typedef int16_t          dr_int16;
typedef uint16_t         dr_uint16;
typedef int32_t          dr_int32;
typedef uint32_t         dr_uint32;
typedef int64_t          dr_int64;
typedef uint64_t         dr_uint64;
#endif
typedef dr_int8          dr_bool8;
typedef dr_int32         dr_bool32;
#define DR_TRUE          1
#define DR_FALSE         0
#endif

// Common data formats.
#define DR_WAVE_FORMAT_PCM          0x1
#define DR_WAVE_FORMAT_ADPCM        0x2     // Not currently supported.
#define DR_WAVE_FORMAT_IEEE_FLOAT   0x3
#define DR_WAVE_FORMAT_ALAW         0x6
#define DR_WAVE_FORMAT_MULAW        0x7
#define DR_WAVE_FORMAT_EXTENSIBLE   0xFFFE

enum drwav_seek_origin {
  drwav_seek_origin_start,
  drwav_seek_origin_current
};

enum drwav_container {
  drwav_container_riff,
  drwav_container_w64
};

// Callback for when data is read. Return value is the number of bytes actually read.
typedef size_t (* drwav_read_proc)(void* pUserData,
				   void* pBufferOut, size_t bytesToRead);

// Callback for when data needs to be seeked. Return value is true on
// success; false on failure.
typedef dr_bool32 (* drwav_seek_proc)(void* pUserData, int offset,
				      drwav_seek_origin origin);

// Structure for internal use. Only used for loaders opened with drwav_open_memory.
struct drwav__memory_stream {
  const unsigned char* data;
  size_t dataSize;
  size_t currentReadPos;
};

struct drwav_fmt {
  // The format tag exactly as specified in the wave file's "fmt"
  // chunk. This can be used by applications that require support for
  // data formats not natively supported by dr_wav.
  unsigned short formatTag;

  // The number of channels making up the audio data. When this is set
  // to 1 it is mono, 2 is stereo, etc.
  unsigned short channels;

  // The sample rate. Usually set to something like 44100.
  unsigned int sampleRate;

  // Average bytes per second. You probably don't need this, but it's
  // left here for informational purposes.
  unsigned int avgBytesPerSec;

  // Block align. This is equal to the number of channels * bytes per sample.
  unsigned short blockAlign;

  // Bits per sample.
  unsigned short bitsPerSample;

  // The size of the extended data. Only used internally for
  // validation, but left here for informational purposes.
  unsigned short extendedSize;

  // The number of valid bits per sample. When <formatTag> is equal to
  // WAVE_FORMAT_EXTENSIBLE, <bitsPerSample> is always rounded up to
  // the nearest multiple of 8. This variable contains information
  // about exactly how many bits a valid per sample. Mainly used for
  // informational purposes.
  unsigned short validBitsPerSample;

  // The channel mask. Not used at the moment.
  unsigned int channelMask;

  // The sub-format, exactly as specified by the wave file.
  unsigned char subFormat[16];
};

struct drwav {
  // A pointer to the function to call when more data is needed.
  drwav_read_proc onRead;

  // A pointer to the function to call when the wav file needs to be seeked.
  drwav_seek_proc onSeek;

  // The user data to pass to callbacks.
  void* pUserData;


  // Whether or not the WAV file is formatted as a standard RIFF file or W64.
  drwav_container container;


  // Structure containing format information exactly as specified by
  // the wav file.
  drwav_fmt fmt;

  // The sample rate. Will be set to something like 44100.
  unsigned int sampleRate;

  // The number of channels. This will be set to 1 for monaural
  // streams, 2 for stereo, etc.
  unsigned short channels;

  // The bits per sample. Will be set to somthing like 16, 24, etc.
  unsigned short bitsPerSample;

  // The number of bytes per sample.
  unsigned short bytesPerSample;

  // Equal to fmt.formatTag, or the value specified by fmt.subFormat
  // if fmt.formatTag is equal to 65534 (WAVE_FORMAT_EXTENSIBLE).
  unsigned short translatedFormatTag;

  // The total number of samples making up the audio data. Use
  // <totalSampleCount> * <bytesPerSample> to calculate
  // the required size of a buffer to hold the entire audio data.
  dr_uint64 totalSampleCount;

    
  // The number of bytes remaining in the data chunk.
  dr_uint64 bytesRemaining;


  // A hack to avoid a malloc() when opening a decoder with drwav_open_memory().
  drwav__memory_stream memoryStream;
};


// Initializes a pre-allocated drwav object.
//
// Returns true if successful; false otherwise.
dr_bool32 drwav_init(drwav* pWav, drwav_read_proc onRead,
		     drwav_seek_proc onSeek, void* pUserData);

// Uninitializes the given drwav object. Use this only for objects
// initialized with drwav_init().
void drwav_uninit(drwav* pWav);


// Opens a wav file using the given callbacks.
//
// Returns null on error. Close the loader with drwav_close().
//
// This is different from drwav_init() in that it will allocate the
// drwav object for you via malloc() before initializing it.
drwav* drwav_open(drwav_read_proc onRead, drwav_seek_proc onSeek,
		  void* pUserData);

// Uninitializes and deletes the the given drwav object. Use this only
// for objects created with drwav_open().
void drwav_close(drwav* pWav);


// Reads raw audio data.
//
// This is the lowest level function for reading audio data. It simply
// reads the given number of bytes of the raw internal sample data.
//
// Returns the number of bytes actually read.
size_t drwav_read_raw(drwav* pWav, size_t bytesToRead, void* pBufferOut);

// Reads a chunk of audio data in the native internal format.
//
// This is typically the most efficient way to retrieve audio data,
// but it does not do any format conversions which means you'll need
// to convert the data manually if required.
//
// If the return value is less than <samplesToRead> it means the end
// of the file has been reached or you have requested more samples
// than can possibly fit in the output buffer.
//
// This function will only work when sample data is of a fixed size.
// If you are using an unusual format which uses variable sized
// samples, consider using drwav_read_raw(), but don't combine them.
dr_uint64 drwav_read(drwav* pWav, dr_uint64 samplesToRead, void* pBufferOut);

// Seeks to the given sample.
//
// The return value is DR_FALSE if an error occurs, DR_TRUE if successful.
dr_bool32 drwav_seek_to_sample(drwav* pWav, dr_uint64 sample);



//// Convertion Utilities ////
#ifndef DR_WAV_NO_CONVERSION_API

// Reads a chunk of audio data and converts it to signed 16-bit PCM samples.
//
// Returns the number of samples actually read.
//
// If the return value is less than <samplesToRead> it means the end
// of the file has been reached.
dr_uint64 drwav_read_s16(drwav* pWav, dr_uint64 samplesToRead,
			 dr_int16* pBufferOut);


// Reads a chunk of audio data and converts it to IEEE 32-bit floating
// point samples.
//
// Returns the number of samples actually read.
//
// If the return value is less than <samplesToRead> it means the end
// of the file has been reached.
dr_uint64 drwav_read_f32(drwav* pWav, dr_uint64 samplesToRead,
			 float* pBufferOut);

// Low-level function for converting unsigned 8-bit PCM samples to
// IEEE 32-bit floating point samples.
void drwav_u8_to_f32(float* pOut, const dr_uint8* pIn, size_t sampleCount);

// Low-level function for converting signed 16-bit PCM samples to IEEE
// 32-bit floating point samples.
void drwav_s16_to_f32(float* pOut, const dr_int16* pIn, size_t sampleCount);

// Low-level function for converting signed 24-bit PCM samples to IEEE
// 32-bit floating point samples.
void drwav_s24_to_f32(float* pOut, const dr_uint8* pIn, size_t sampleCount);

// Low-level function for converting signed 32-bit PCM samples to IEEE
// 32-bit floating point samples.
void drwav_s32_to_f32(float* pOut, const dr_int32* pIn, size_t sampleCount);

// Low-level function for converting IEEE 64-bit floating point
// samples to IEEE 32-bit floating point samples.
void drwav_f64_to_f32(float* pOut, const double* pIn, size_t sampleCount);

// Low-level function for converting A-law samples to IEEE 32-bit
// floating point samples.
void drwav_alaw_to_f32(float* pOut, const dr_uint8* pIn, size_t sampleCount);

// Low-level function for converting u-law samples to IEEE 32-bit
// floating point samples.
void drwav_ulaw_to_f32(float* pOut, const dr_uint8* pIn, size_t sampleCount);


// Reads a chunk of audio data and converts it to signed 32-bit PCM samples.
//
// Returns the number of samples actually read.
//
// If the return value is less than <samplesToRead> it means the end
// of the file has been reached.
dr_uint64 drwav_read_s32(drwav* pWav, dr_uint64 samplesToRead,
			 dr_int32* pBufferOut);

// Low-level function for converting unsigned 8-bit PCM samples to
// signed 32-bit PCM samples.
void drwav_u8_to_s32(dr_int32* pOut, const dr_uint8* pIn, size_t sampleCount);

// Low-level function for converting signed 16-bit PCM samples to
// signed 32-bit PCM samples.
void drwav_s16_to_s32(dr_int32* pOut, const dr_int16* pIn, size_t sampleCount);

// Low-level function for converting signed 24-bit PCM samples to
// signed 32-bit PCM samples.
void drwav_s24_to_s32(dr_int32* pOut, const dr_uint8* pIn, size_t sampleCount);

// Low-level function for converting IEEE 32-bit floating point
// samples to signed 32-bit PCM samples.
void drwav_f32_to_s32(dr_int32* pOut, const float* pIn, size_t sampleCount);

// Low-level function for converting IEEE 64-bit floating point
// samples to signed 32-bit PCM samples.
void drwav_f64_to_s32(dr_int32* pOut, const double* pIn, size_t sampleCount);

// Low-level function for converting A-law samples to signed 32-bit PCM samples.
void drwav_alaw_to_s32(dr_int32* pOut, const dr_uint8* pIn, size_t sampleCount);

// Low-level function for converting u-law samples to signed 32-bit PCM samples.
void drwav_ulaw_to_s32(dr_int32* pOut, const dr_uint8* pIn, size_t sampleCount);

#endif  //DR_WAV_NO_CONVERSION_API


//// High-Level Convenience Helpers ////

#ifndef DR_WAV_NO_STDIO

// Helper for initializing a wave file using stdio.
//
// This holds the internal FILE object until drwav_uninit() is called.
// Keep this in mind if you're caching drwav objects because the
// operating system may restrict the number of file handles an
// application can have open at any given time.
dr_bool32 drwav_init_file(drwav* pWav, const char* filename);

// Helper for opening a wave file using stdio.
//
// This holds the internal FILE object until drwav_close() is called.
// Keep this in mind if you're caching drwav objects because the
// operating system may restrict the number of file handles an
// application can have open at any given time.
drwav* drwav_open_file(const char* filename);

#endif  // DR_WAV_NO_STDIO

// Helper for initializing a file from a pre-allocated memory buffer.
//
// This does not create a copy of the data. It is up to the
// application to ensure the buffer remains valid for the lifetime of
// the drwav object.
//
// The buffer should contain the contents of the entire wave file, not
// just the sample data.
dr_bool32 drwav_init_memory(drwav* pWav, const void* data, size_t dataSize);

// Helper for opening a file from a pre-allocated memory buffer.
//
// This does not create a copy of the data. It is up to the
// application to ensure the buffer remains valid for the lifetime of
// the drwav object.
//
// The buffer should contain the contents of the entire wave file, not
// just the sample data.
drwav* drwav_open_memory(const void* data, size_t dataSize);



#ifndef DR_WAV_NO_CONVERSION_API
// Opens and reads a wav file in a single operation.
dr_int16* drwav_open_and_read_s16(
    drwav_read_proc onRead, drwav_seek_proc onSeek,
    void* pUserData, unsigned int* channels,
    unsigned int* sampleRate,
    dr_uint64* totalSampleCount);
float* drwav_open_and_read_f32(
    drwav_read_proc onRead, drwav_seek_proc onSeek,
    void* pUserData, unsigned int* channels,
    unsigned int* sampleRate, dr_uint64* totalSampleCount);
dr_int32* drwav_open_and_read_s32(
    drwav_read_proc onRead, drwav_seek_proc onSeek,
    void* pUserData, unsigned int* channels,
    unsigned int* sampleRate,
    dr_uint64* totalSampleCount);
#ifndef DR_WAV_NO_STDIO
// Opens an decodes a wav file in a single operation.
dr_int16* drwav_open_and_read_file_s16(
    const char* filename, unsigned int* channels,
    unsigned int* sampleRate,
    dr_uint64* totalSampleCount);
float* drwav_open_and_read_file_f32(
    const char* filename, unsigned int* channels,
    unsigned int* sampleRate,
    dr_uint64* totalSampleCount);
dr_int32* drwav_open_and_read_file_s32(
    const char* filename, unsigned int* channels,
    unsigned int* sampleRate,
    dr_uint64* totalSampleCount);
#endif

// Opens an decodes a wav file from a block of memory in a single operation.
dr_int16* drwav_open_and_read_memory_s16(const void* data, size_t dataSize,
					 unsigned int* channels,
					 unsigned int* sampleRate,
					 dr_uint64* totalSampleCount);
float* drwav_open_and_read_memory_f32(const void* data, size_t dataSize,
				      unsigned int* channels,
				      unsigned int* sampleRate,
				      dr_uint64* totalSampleCount);
dr_int32* drwav_open_and_read_memory_s32(const void* data, size_t dataSize,
					 unsigned int* channels,
					 unsigned int* sampleRate,
					 dr_uint64* totalSampleCount);
#endif

// Frees data that was allocated internally by dr_wav.
void drwav_free(void* pDataReturnedByOpenAndRead);

#endif  // __DR_WAV_H

/*
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>
*/
