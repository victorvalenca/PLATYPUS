# PLATYPUS
*Buffer Descriptor for the PLATYPUS language specification*

## Building

### In macOS/Linux
#### Requires
*gcc (tested in version 6.3.x) or clang (tested under LLVM clang-0800.0.42.1)*

- `make [gcc | clang]`
    - Default is to build both versions (this is an aftermath of testing behaviour from different compilers)
    - It is safe to modify the default to either gcc or clang if you don't have one of them

### Windows
#### Requires
- `msvc` (tested under Visual Studio 2015 with Update 3, however VS2013 shouldn't behave abnormally either)
- Disable language extensions (use ANSI C)
---
## Running

- *macOS/linux*
    - `$ ./buffer_<compiler> <file> [f | a | m] > <out_file>`
- *Windows*
    - `$ buffer <file> f | a | m] > <out_file>`

### Usage
- F: fixed capacity mode. Buffe will not auto-resize if not enough space for the file
- A: additive mode: Buffer will resize in a linear increment.
- M: multiplicative mode: Buffer will resize based on a pre-set multiplier based on remaining allowed memory space (`$SHRT_MAX bytes`)


# Notes

This is in no way complete, it is missing a scanner implementation, parser, symbol table, etc.

You can modify the initial capacity and increment factor defined in `platy_bt.c` (should really make that a command line parameter in a future release)
- Increment factor range for additive mode: `1 - 255`. (setting this to 0 implies fixed mode, regardless of the mode given in the command line)
- Initial capacity range (in bytes): `0 - 32767 (SHRT_MAX)`
- This is due to my environment's install locations for the C include libraries: 
    - **Change the `#include` statement from `mm_malloc.h` to `malloc.h`, depending on what your compiler uses. Homebrew gcc6 installs on macOS might need this, but Linux and Windows generally will use `malloc.h`**
