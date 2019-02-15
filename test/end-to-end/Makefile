# Build directory path
build = ./build
# Fortran Compiler
fc = gfortran
# Fortran Compiler flags
# -J specifies where to put .mod files for compiled modules
development_flags = -J$(build) -std=f2008 -pedantic -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=all -fbacktrace
production_falgs  = -J$(build) -std=f2008 -pedantic -Wall -Wextra -Wimplicit-interface -fPIC -Werror -fmax-errors=1 -O3 -march=native -ffast-math -funroll-loops
flags = $(development_flags)

# If not exist, create build directory
$(shell mkdir -p $(build))

# clifor variable from file
include ../../dependencies.make
# Object files relative to each source file in $(clifor)
clifor.obj = $(patsubst %, $(build)/%.o, $(clifor))

default: example
	@./run-tests.sh

example: $(clifor.obj) $(build)/example.o
	$(fc) $(flags) -o $(@) $(^)

$(build)/example.o: example.f08
	$(fc) $(flags) -c $(<) -o $(@)

# Compile clifor files
$(build)/%.o: ../../source/%.f08
	$(fc) $(flags) -c $(<) -o $(@)

clean:
	rm -rf $(build)
