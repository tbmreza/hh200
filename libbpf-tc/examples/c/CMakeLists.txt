# SPDX-License-Identifier: GPL-2.0 OR BSD-3-Clause

cmake_minimum_required(VERSION 3.16)
project(examples C)

# Tell cmake where to find BpfObject module
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../../tools/cmake)

# Build vendored libbpf
include(ExternalProject)
ExternalProject_Add(libbpf
  PREFIX libbpf
  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../libbpf/src
  CONFIGURE_COMMAND ""
  BUILD_COMMAND make
    CC=${CMAKE_C_COMPILER}
    BUILD_STATIC_ONLY=1
    OBJDIR=${CMAKE_CURRENT_BINARY_DIR}/libbpf/libbpf
    DESTDIR=${CMAKE_CURRENT_BINARY_DIR}/libbpf
    INCLUDEDIR=
    LIBDIR=
    UAPIDIR=
    install install_uapi_headers
  BUILD_IN_SOURCE TRUE
  INSTALL_COMMAND ""
  STEP_TARGETS build
  BUILD_BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/libbpf/libbpf.a
)

ExternalProject_Add(bpftool
  PREFIX bpftool
  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../bpftool/src
  CONFIGURE_COMMAND ""
  BUILD_COMMAND make bootstrap
    OUTPUT=${CMAKE_CURRENT_BINARY_DIR}/bpftool/
  BUILD_IN_SOURCE TRUE
  INSTALL_COMMAND ""
  STEP_TARGETS build
)

# Set BpfObject input parameters
if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "x86_64")
  set(ARCH "x86")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "arm")
  set(ARCH "arm")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "aarch64")
  set(ARCH "arm64")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le")
  set(ARCH "powerpc")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "mips")
  set(ARCH "mips")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "riscv64")
  set(ARCH "riscv")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "loongarch64")
  set(ARCH "loongarch")
endif()

set(BPFOBJECT_BPFTOOL_EXE ${CMAKE_CURRENT_BINARY_DIR}/bpftool/bootstrap/bpftool)
set(BPFOBJECT_VMLINUX_H ${CMAKE_CURRENT_SOURCE_DIR}/../../vmlinux.h/include/${ARCH}/vmlinux.h)
set(LIBBPF_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/libbpf)
set(LIBBPF_LIBRARIES ${CMAKE_CURRENT_BINARY_DIR}/libbpf/libbpf.a)
find_package(BpfObject REQUIRED)

# Build tc example
bpf_object(tc tc.bpf.c)
add_dependencies(tc_skel libbpf bpftool)

add_executable(tc tc.c)
target_link_libraries(tc tc_skel)