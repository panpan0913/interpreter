cmake_minimum_required(VERSION 3.10)
project(InterpreterProject)

set(CMAKE_CXX_STANDARD 17)

add_executable(Interpreter interpreter.cpp)
target_link_libraries(Interpreter pthread)