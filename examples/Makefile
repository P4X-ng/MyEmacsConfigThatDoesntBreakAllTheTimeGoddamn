# Example Makefile to test autocomplete
# This file demonstrates Makefile completion features

# Variables - try typing part of these names and see completions
CC = gcc
CFLAGS = -Wall -Wextra -O2
LDFLAGS = -lm
TARGET = myprogram
SOURCES = main.c utils.c helper.c
OBJECTS = $(SOURCES:.c=.o)

# Phony targets
.PHONY: all clean install test

# Default target
all: $(TARGET)

# Link the target - try typing $(OB and see completion to $(OBJECTS)
$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJECTS) $(LDFLAGS)

# Compile C files - try typing $(C and see completion to $(CC) or $(CFLAGS)
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean build artifacts - try typing $(TAR and see completion to $(TARGET)
clean:
	rm -f $(OBJECTS) $(TARGET)

# Install - try typing /usr/local/ and see file path completions
install: $(TARGET)
	install -m 755 $(TARGET) /usr/local/bin/

# Run tests - try typing tes and see completion to test target
test: $(TARGET)
	./$(TARGET) --test

# Help target - shows all available targets
help:
	@echo "Available targets:"
	@echo "  all     - Build the program (default)"
	@echo "  clean   - Remove build artifacts"
	@echo "  install - Install to /usr/local/bin"
	@echo "  test    - Run tests"
	@echo "  help    - Show this help message"
