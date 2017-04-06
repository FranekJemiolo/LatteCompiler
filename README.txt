Author: Franciszek Jemioło index number 346919

This project contains Latte compiler to LLVM.

Usage:
make -  compiles sources and creates latc_llvm in make dir
make clean - cleans the project

latc_llvm file.lat - creates file.ll and file.bc

Sources in src dir
The built-in functions are defined in lib/runtime.c and compile to runtime.ll and runtime.bc with 
	make so please link with llvm-link after compilation to use these functions.

Frontend:
Base version + Arrays + Classes + Some const expression optimization + Removing some dead code

Backend (LLVM only):
Base version + Arrays + (Classes with virtual methods)

I assumed that there is no comparing of strings.
In classes names of the functions I did not perform some special name mangling - just used the 
simple layout "class." ++ className ++ ".vtable." ++ methodName.

For class type definition: "class." ++ className

