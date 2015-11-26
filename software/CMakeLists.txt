

# svm_large_data CMakeLists.txt
#       


# --- cmake settings
#
    cmake_minimum_required (VERSION 2.8)
    include(ExternalProject)

    
# --- disallow in source builds
    set(CMAKE_DISABLE_SOURCE_CHANGES ON)
    set(CMAKE_DISABLE_IN_SOURCE_BUILD ON)

#???    include_directories("${PROJECT_SOURCE_DIR}/SVMperf/src")

# --- avoid source tree pollution
#
	If(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
		message(FATAL_ERROR "In-source builds are not permitted. Make a separate folder for building:\nmkdir build; cd build; cmake ..\nBefore that, remove the files already created:\nrm -rf CMakeCache.txt CMakeFiles")
	endif(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)



    
# --- project settings
#
    PROJECT (svm_large_data)
  

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



# --- find dependencies
#
  IF (UNIX)
    FIND_PACKAGE(PkgConfig)
  ENDIF (UNIX)
 
  
  
  
# --- copy and compile
# 

#set (AESVM "AESVM/aesvm-predict" "AESVM/aesvm-train")
set (BudgetedSVM "bin/budgetedsvm-predict" "bin/budgetedsvm-train")
set (LASVM "la_svm" "la_test")
set (libCVM "libCVM/svm-train" "libCVM/svm-predict")
set (LIBSVM "svm-train" "svm-predict")
set (SVMperf "svm_perf_classify" "svm_perf_learn")
set (SharkSVM "SharkSVM-train" "SharkSVM-predict")


foreach(PROJECT "BudgetedSVM" "LASVM" "libCVM" "LIBSVM" "SVMperf") # "ISSVM") #"AESVM"

  message ("     Preparing ${PROJECT}")
  
  # collect all files in that project
  file(GLOB files "${CMAKE_CURRENT_BINARY_DIR}/../${PROJECT}/src/*")

  # create external project
  ExternalProject_Add("${PROJECT}"
		    PREFIX "${PROJECT}_build"
		    BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT}_bin
		    CONFIGURE_COMMAND ""
		    DOWNLOAD_COMMAND "/bin/cp" -R ${files} . 
		    DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT}_bin
		    BUILD_COMMAND pwd COMMAND /usr/bin/make
		    INSTALL_COMMAND "mkdir" -p ${CMAKE_CURRENT_BINARY_DIR}/../${PROJECT}/bin COMMAND "/bin/cp" -R ${${PROJECT}} "${CMAKE_CURRENT_BINARY_DIR}/../${PROJECT}/bin/"
		    )
ENDFOREACH(PROJECT)


#add_executable(myexecutable myexcutable.c)
#target_link_libraries(myexecutable full_path_to_generated_library)
#add_dependencies(myexecutable extern_lib)
        
        
