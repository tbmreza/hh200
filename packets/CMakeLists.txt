cmake_minimum_required(VERSION 4.3)
project(Packets LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)


add_library(libname add.cpp)


add_executable(test_add test_add.cpp)
target_link_libraries(test_add PRIVATE libname)


enable_testing()
add_test(NAME add_test COMMAND test_add)
