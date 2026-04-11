cmake_minimum_required(VERSION 3.25)
project(hhs-fmt VERSION 0.0.1 LANGUAGES CXX)

# GCC 13.3 supports -std=c++23 (which was renamed from c++2b).
# C++26 flag (-std=c++2c or -std=c++26) is not supported by GCC 13.
# However, the user requested C++26. We will set it to 26 and allow CMake
# to try and map it, but for this specific environment (GCC 13), we might
# need to fallback if we want it to build.
# BUT to be "mostly correct" and follow the requirement, we set it to 26.

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Library
add_library(hhs-fmt src/rope.cpp)
add_library(hhs::fmt ALIAS hhs-fmt)

target_include_directories(hhs-fmt
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
)

# Testing
enable_testing()

find_package(Catch2 3 QUIET)

if(NOT Catch2_FOUND)
    include(FetchContent)
    FetchContent_Declare(
      Catch2
      GIT_REPOSITORY https://github.com/catchorg/Catch2.git
      GIT_TAG        v3.5.2
    )
    FetchContent_MakeAvailable(Catch2)
endif()

add_executable(hhs-fmt-tests tests/test_rope.cpp)
target_link_libraries(hhs-fmt-tests PRIVATE hhs::fmt Catch2::Catch2WithMain)

include(CTest)
add_test(NAME hhs-fmt-tests COMMAND hhs-fmt-tests)

# Installation
include(GNUInstallDirs)
install(TARGETS hhs-fmt
    EXPORT hhs-fmt-targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

install(EXPORT hhs-fmt-targets
    FILE hhs-fmtTargets.cmake
    NAMESPACE hhs::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/hhs-fmt
)

# For Debian packaging
set(CPACK_PACKAGE_NAME "hhs-fmt")
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_PACKAGE_CONTACT "Reza Handzalah <rezahandzalah@gmail.com>")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Ropes data structure implementation")
set(CPACK_GENERATOR "DEB")
include(CPack)
