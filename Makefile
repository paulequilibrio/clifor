# Build directory path
build = ../build
# Fortran Compiler
fc = gfortran
# Fortran Compiler flags
# -J specifies where to put .mod files for compiled modules
clifor_flags  = -J$(build) -std=f2008 -pedantic -Wall -Wextra -Wimplicit-interface -fPIC -Werror -fmax-errors=1 -O3 -march=native -ffast-math -funroll-loops

# If not exist, create build directory
$(shell mkdir -p $(build))

# clifor variable from file
include ./dependencies.make
# Object files relative to each source file in $(clifor)
clifor.obj = $(patsubst %, $(build)/%.o, $(clifor))


default: $(clifor.obj)

# Compile clifor files
$(build)/%.o: source/%.f08
	$(fc) $(clifor_flags) -c $(<) -o $(@)

clean:
	rm -rf $(build)
