
# libCVM CMakeLists.txt
#       modified the original sources to be cmake compatible.

# --- cmake settings
#
    cmake_minimum_required (VERSION 2.8)


# --- disallow in source builds
#    set(CMAKE_DISABLE_SOURCE_CHANGES ON)
#    set(CMAKE_DISABLE_IN_SOURCE_BUILD ON)

    
# --- project settings
#
    PROJECT (libCVM)
  

# --- specific os things, fix later.
#
  IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    # linux specific code
    SET(OperatingSystem "Linux")
    message("-- Linux build detected.")
  ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux") 


  IF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    # OSX specific code
    SET(OperatingSystem "Mac OS X")
    message("-- OSX build detected.")
  ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin") 


  if(WIN32)
    # windows specific code
    SET(OperatingSystem "Win")
    message("-- Windows build detected.")
  endif(WIN32)




# --- avoid source tree pollution
#
#	If(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
#		message(FATAL_ERROR "In-source builds are not permitted. Make a separate folder for building:\nmkdir build; cd build; cmake ..\nBefore that, remove the files already created:\nrm -rf CMakeCache.txt CMakeFiles")
#	endif(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)




# --- find dependencies
#
	
	IF (UNIX)
	  FIND_PACKAGE(PkgConfig)
	ENDIF (UNIX)

	
	

	add_definitions(-W -ggdb -Wall)

	
#        set(PROJECT_BINARY_DIR ${PROJECT_SOURCE_DIR}/bin)
        set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR})

        include_directories(${CMAKE_SOURCE_DIR}/libCVM)
        link_directories(${CMAKE_BINARY_DIR}/libCVM)




# --- library

        add_subdirectory(libCVM)


        
# --- executables

        add_executable (svm-train svm-train.cpp)
        add_executable (svm-predict svm-predict.cpp)
        target_link_libraries (svm-train libCVM)
        target_link_libraries (svm-predict libCVM)
        
        