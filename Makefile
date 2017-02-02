## CLANG flags
CLANG = clang

CL_SANITIZE = -fsanitize=address
CL_FLAGS = -fno-omit-frame-pointer

## GCC flags
GCC = gcc-6
GCC_FLAGS = -Wall -ansi -pedantic

## Target executable filename, source, dependencies
SRC = buffer.c platy_bt.c
DEPS = buffer.h
ALL = $(SRC) $(DEPS)

# Object files
OBJ = buffer.o platy_bt.o

.PHONY : gcc
gcc : $(ALL)
	$(GCC) $(ALL) -o buffer_gcc

.PHONY : clang
clang : $(OBJ)
	$(CLANG) -g $(CL_SANITIZE) $(OBJ) -o buffer_clang

# Used exclusively by clang
*.o: $(ALL)
	$(CLANG) -g $(CL_SANITIZE) $(CL_FLAGS) $(SRC)

.PHONY : clean
clean :
	rm *.o