# Makefile for building Schematra & friends

# Compiler and flags
CSC = csc
CSCFLAGS = -shared -J

# Module names
MODULES = chiccup schematra chiccup sessions oauthtoothy

# Default target
all: $(addsuffix .so, $(MODULES))

# Generic rule for building .so files from .scm files
%.so: %.scm
	$(CSC) $(CSCFLAGS) $<

# Clean target
clean:
	rm -f *.so *.import.scm *.link

# Install target (optional - installs to chicken repository)
install: all
	chicken-install -s $(MODULES)

# Development target - builds and runs demo
demo: all
	csi -s demo.scm

.PHONY: all clean install demo
