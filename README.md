# PLATYPUS
*Frontend compiler for the PLATYPUS language specification*

## Building

### In macOS/Linux
#### Requires
*gcc (tested in version 6.3.x) or clang (tested under LLVM clang-0800.0.42.1)*

- `make [gcc | clang]`
    - No argument will build both versions (this is an aftermath of having to check behaviour in the binary from multiple compilers)
    - It is safe to modify the default to either gcc or clang if you wish. ** SEE NOTES BELOW **
- **NOTE: I haven't tested or created make targets for the final implementation yet. This is somewhat buried deep into the backburner**
### Windows
#### Requires
- `msvc` (tested under Visual Studio 2015 with Update 3, however VS 2012 and 2013 shouldn't behave abnormally either, nor should 2017)
- Disable language extensions (use ANSI C)
---
## Running
### Running the Buffer only
- *macOS/linux*
    - `$ ./buffer_<compiler> <file> [f | a | m] > <out_file>`
- *Windows*
    - `$ buffer <file> f | a | m] > <out_file>`

### Usage
- F: fixed capacity mode. Buffe will not auto-resize if not enough space for the file
- A: additive mode: Buffer will resize in a linear increment.
- M: multiplicative mode: Buffer will resize based on a pre-set multiplier based on remaining allowed memory space (`$SHRT_MAX bytes`)


# Notes

**This is in no way complete, or ready for production in any shape or form.** It works up to the parser, but still contains incorrect grammar parsing and some edge cases that causes crashes.

You can modify the initial capacity and increment factor defined in `platy.c` (should really make that a command line parameter in a future release)
- Increment factor range for additive mode: `1 - 255`. (setting this to 0 implies fixed mode, regardless of the mode given in the command line)
- Initial capacity range (in bytes): `0 - 32767 (SHRT_MAX)`
- This is an issue caused by my environment's install locations for the C include libraries: 
    - **`buffer.h` contains an `#ifdef` directive checking if `WIN32` exists. Due to an upgrade issue with GCC in macOS (possibly only in my machine), `malloc.h` is actually named `mm_maloc.h`. If your system uses `malloc.h`, you can safely delete the check and use the regular filename instead.**
