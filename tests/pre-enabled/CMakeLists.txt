cmake_minimum_required(VERSION 3.20)
project(cloudmatrix_pawn_review C)
if(NOT DEFINED PAWN_SOURCE)
  message(FATAL_ERROR "Pass the extracted, verified Pawn 4.1.7152 source as PAWN_SOURCE")
endif()
# Compile unchanged CLI sources from the pinned source RPM. Its Windows .rc
# references an absent icon/version macro; no resource is needed for the CLI.
set(compiler_sources sc1 sc2 sc3 sc4 sc5 sc6 sc7 scexpand sci18n sclist
    scmemfil scstate scvars lstring memfile)
set(sources)
foreach(name IN LISTS compiler_sources)
  list(APPEND sources "${PAWN_SOURCE}/compiler/${name}.c")
endforeach()
add_executable(spd_amx_verify spd_amx_verify.c "${PAWN_SOURCE}/amx/amx.c")
target_include_directories(spd_amx_verify PRIVATE "${PAWN_SOURCE}/amx")
target_compile_definitions(spd_amx_verify PRIVATE PAWN_CELL_SIZE=64 AMX_NODYNALOAD AMX_NO_PACKED_OPC
    AMX_ANSIONLY HAVE_STDINT_H HAVE_INTTYPES_H _CRT_SECURE_NO_WARNINGS)
add_executable(pawncc ${sources} "${PAWN_SOURCE}/amx/keeloq.c")
add_executable(pawndisasm "${PAWN_SOURCE}/compiler/pawndisasm.c")
foreach(target pawncc pawndisasm)
  target_compile_definitions(${target} PRIVATE HAVE_STDINT_H HAVE_INTTYPES_H _CRT_SECURE_NO_WARNINGS)
endforeach()
