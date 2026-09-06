# SPDX-License-Identifier: LGPL-2.1-or-later
cmake_minimum_required(VERSION 3.15)
project(Ddr5ReadOnlyMockChecks C)
set(CMAKE_C_STANDARD 99)
if(NOT EXISTS "${PAWN_SOURCE}/compiler/sc1.c")
  message(FATAL_ERROR "PAWN_SOURCE must reference the verified Pawn 4.1.7152 source archive")
endif()
set(cc_names sc1 sc2 sc3 sc4 sc5 sc6 sc7 scexpand sci18n sclist scmemfil scstate scvars lstring memfile)
set(cc_sources "${PAWN_SOURCE}/amx/keeloq.c")
foreach(name IN LISTS cc_names)
  if(name STREQUAL "sc3")
    # Legacy declarations qualify void return types, incompatible with modern GCC.
    file(READ "${PAWN_SOURCE}/compiler/sc3.c" sc3_source)
    string(REPLACE "const void (*" "void (*" sc3_source "${sc3_source}")
    file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/sc3-compatible.c" "${sc3_source}")
    list(APPEND cc_sources "${CMAKE_CURRENT_BINARY_DIR}/sc3-compatible.c")
  else()
    list(APPEND cc_sources "${PAWN_SOURCE}/compiler/${name}.c")
  endif()
endforeach()
# No installer/IDE/icon resources; only the compiler and a user-mode mock host.
add_executable(pawncc ${cc_sources})
target_include_directories(pawncc PRIVATE "${PAWN_SOURCE}/compiler")
target_compile_definitions(pawncc PRIVATE HAVE_INTTYPES_H HAVE_STDINT_H HAVE_UNISTD_H HAVE_ALLOCA_H=0)
# Upstream's 64-bit VM assumes LP64 unsigned long. Windows is LLP64.
# Generate a local test-host copy with cell-width shifts; never alter module bytecode.
file(READ "${PAWN_SOURCE}/amx/amx.c" amx_source)
string(REPLACE "1UL << sizeof(cell)*4" "((ucell)1) << sizeof(cell)*4" amx_source "${amx_source}")
string(REPLACE "1L<<(sizeof(cell)*4)" "((ucell)1)<<(sizeof(cell)*4)" amx_source "${amx_source}")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/amx-llp64.c" "${amx_source}")
add_executable(readonly-module-tests module-tests.c
  "${CMAKE_CURRENT_BINARY_DIR}/amx-llp64.c" "${PAWN_SOURCE}/amx/amxaux.c")
target_include_directories(readonly-module-tests PRIVATE "${PAWN_SOURCE}/amx")
target_compile_definitions(readonly-module-tests PRIVATE
  PAWN_CELL_SIZE=64 AMX_NODYNALOAD AMX_ANSIONLY HAVE_INTTYPES_H HAVE_STDINT_H HAVE_ALLOCA_H=0)
