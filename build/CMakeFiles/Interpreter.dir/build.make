# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.16

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/zhangpan/桌面/interpreter

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/zhangpan/桌面/interpreter/build

# Include any dependencies generated for this target.
include CMakeFiles/Interpreter.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/Interpreter.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/Interpreter.dir/flags.make

CMakeFiles/Interpreter.dir/interpreter.cpp.o: CMakeFiles/Interpreter.dir/flags.make
CMakeFiles/Interpreter.dir/interpreter.cpp.o: ../interpreter.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/zhangpan/桌面/interpreter/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/Interpreter.dir/interpreter.cpp.o"
	/usr/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Interpreter.dir/interpreter.cpp.o -c /home/zhangpan/桌面/interpreter/interpreter.cpp

CMakeFiles/Interpreter.dir/interpreter.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Interpreter.dir/interpreter.cpp.i"
	/usr/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/zhangpan/桌面/interpreter/interpreter.cpp > CMakeFiles/Interpreter.dir/interpreter.cpp.i

CMakeFiles/Interpreter.dir/interpreter.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Interpreter.dir/interpreter.cpp.s"
	/usr/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/zhangpan/桌面/interpreter/interpreter.cpp -o CMakeFiles/Interpreter.dir/interpreter.cpp.s

# Object files for target Interpreter
Interpreter_OBJECTS = \
"CMakeFiles/Interpreter.dir/interpreter.cpp.o"

# External object files for target Interpreter
Interpreter_EXTERNAL_OBJECTS =

Interpreter: CMakeFiles/Interpreter.dir/interpreter.cpp.o
Interpreter: CMakeFiles/Interpreter.dir/build.make
Interpreter: CMakeFiles/Interpreter.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/zhangpan/桌面/interpreter/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable Interpreter"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/Interpreter.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/Interpreter.dir/build: Interpreter

.PHONY : CMakeFiles/Interpreter.dir/build

CMakeFiles/Interpreter.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/Interpreter.dir/cmake_clean.cmake
.PHONY : CMakeFiles/Interpreter.dir/clean

CMakeFiles/Interpreter.dir/depend:
	cd /home/zhangpan/桌面/interpreter/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/zhangpan/桌面/interpreter /home/zhangpan/桌面/interpreter /home/zhangpan/桌面/interpreter/build /home/zhangpan/桌面/interpreter/build /home/zhangpan/桌面/interpreter/build/CMakeFiles/Interpreter.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/Interpreter.dir/depend

