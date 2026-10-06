function(configure_sanitizers requested_sanitizers)
  if(NOT requested_sanitizers)
    return()
  endif()

  include(CheckFortranCompilerFlag)

  # End-to-end compile AND link check for general sanitizer support
  set(CMAKE_REQUIRED_LINK_OPTIONS "-fsanitize=address")
  check_fortran_compiler_flag("-fsanitize=address" Fortran_SUPPORTS_SANITIZERS)
  unset(CMAKE_REQUIRED_LINK_OPTIONS)

  if(NOT Fortran_SUPPORTS_SANITIZERS)
    message(WARNING
      "Sanitizers requested (-DUSE_SANITIZERS='${requested_sanitizers}'), but Fortran compiler '${CMAKE_Fortran_COMPILER_ID}' failed sanitizer link checks. Sanitizers will be ignored."
    )
    return()
  endif()

  # Normalize options to uppercase
  set(sanitizers_upper "")
  foreach(item IN LISTS requested_sanitizers)
    string(TOUPPER "${item}" item_upper)
    list(APPEND sanitizers_upper "${item_upper}")
  endforeach()

  # Enforce mutual exclusion rules
  if("THREAD" IN_LIST sanitizers_upper)
    if("ADDRESS" IN_LIST sanitizers_upper OR "LEAK" IN_LIST sanitizers_upper OR "MEMORY" IN_LIST sanitizers_upper)
      message(FATAL_ERROR "ThreadSanitizer (TSan) cannot be combined with Address, Leak, or Memory sanitizers in Fortran.")
    endif()
  endif()

  if("MEMORY" IN_LIST sanitizers_upper)
    if("ADDRESS" IN_LIST sanitizers_upper OR "LEAK" IN_LIST sanitizers_upper)
      message(FATAL_ERROR "MemorySanitizer (MSan) cannot be combined with Address or Leak sanitizers in Fortran.")
    endif()
  endif()

  # Assemble flags based on validated selections
  set(sanitizer_flags "")
  foreach(sanitizer IN LISTS sanitizers_upper)
    if(sanitizer STREQUAL "ADDRESS")
      list(APPEND sanitizer_flags "address")
    elseif(sanitizer STREQUAL "LEAK")
      list(APPEND sanitizer_flags "leak")
    elseif(sanitizer STREQUAL "UNDEFINED")
      list(APPEND sanitizer_flags "undefined")
    elseif(sanitizer STREQUAL "THREAD")
      list(APPEND sanitizer_flags "thread")
    elseif(sanitizer STREQUAL "MEMORY")
      if(CMAKE_Fortran_COMPILER_ID MATCHES "LLVMFlang|Flang|Clang")
        list(APPEND sanitizer_flags "memory")
        add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-fsanitize-memory-track-origins>)
      else()
        message(FATAL_ERROR "MemorySanitizer (MSan) for Fortran requires LLVM Flang.")
      endif()
    else()
      message(FATAL_ERROR "Unknown Fortran sanitizer option: '${sanitizer}'")
    endif()
  endforeach()

  # Apply flags strictly to Fortran targets and linker
  if(sanitizer_flags)
    list(JOIN sanitizer_flags "," sanitizer_arg)
    add_compile_options(
      $<$<COMPILE_LANGUAGE:Fortran>:-fsanitize=${sanitizer_arg}>
      $<$<COMPILE_LANGUAGE:Fortran>:-fno-omit-frame-pointer>
    )
    add_link_options(-fsanitize=${sanitizer_arg})
  endif()
endfunction()
