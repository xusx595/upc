Instructions for building the translator:

1.  cd $HOME/compilers/open64/osprey1.0/targia32_ia64_nodebug (this is the build directory)

2.  run gmake on the following subdirectories:  
    driver.nue/  (the main compiler driver)
    gccfe/       (the front end)
    be/          (the back end)
    whirl2c/
    
3.  Go to driver.nue/, and do 
    ln -s driver sgiupc

To run the translator, you can modify the following script:

######################
#!/bin/bash

$COMPILER/targia32_ia64_nodebug/driver.nue/sgiupc -save-temps -S -keep -show -fconfig-/home/ftg1/wychen/compilers/open64/osprey1.0/testsuit
e/foo -I$COMPILER/libupc/include -I../common -I$COMPILER/libupc -Yf,$COMPILER/targia32_ia64_nodebug/gccfe -Yb,$COMPILER/targia32_ia64_nodeb
ug/be -Wb,-trlow $1.c

if test $? -eq 0 
then
    $COMPILER/targia32_ia64_nodebug/whirl2c/whirl2c -lang=upc -std=upc $1.c -fB,$1.N    
else
    echo
fi
######################

The output of the compiler should be two C files: <file_name>.w2c.h and <file_name>.w2c.c

Note that the "-fconfig-<file>" option is used to specify the configuration file with information about size of shared pointers, etc.

To compile with a static number of threads, pass the option "-fupc-threads-N" to sgiupc, where N is the number of threads.



