
#
# Add files for libCVM
#

SET(LIBCVM_SOURCE
    bvm.cpp  
    cvm_core.cpp  
    cvm.cpp
    random.cpp  
    sgraph.cpp  
    svm.cpp  
    utility.cpp  
    )
    
    


SET(LIBCVM_HEADER
    bvm.h  
    cvm.h  
    random.h  
    sgraph.h  
    svm.h  
    utility.h
)



# --- only allow static library for now

        add_library(libCVM STATIC ${LIBCVM_SOURCE} ${LIBCVM_HEADER})

#    target_link_libraries(libCVM ${QT_LIBRARIES}) # ${Boost_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
        install(TARGETS libCVM DESTINATION libCVM)        

