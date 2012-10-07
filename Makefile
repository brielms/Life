# Makefile to compile example program 'Gravity'

F90 = gfortran-mp-4.7
F90FLAGS = -cpp -O3
#F90FLAGS = -cpp -O3 -fbounds-check
#F90FLAGS = -cpp -O3 -g -fopenmp

#F90 = ifort
#F90FLAGS = -cpp -fast
#F90FLAGS = -cpp -fast -openmp -openmp-report1
#F90FLAGS = -cpp -fast -parallel -par-report2
#F90FLAGS = -cpp -fast -g


F90CFLAGS = -c
F90LFLAGS =

F90_COMPILE  = $(F90) $(F90FLAGS) $(F90CFLAGS)
F90_LOAD     = $(F90) $(F90FLAGS) $(F90LFLAGS)

.SUFFIXES:
.SUFFIXES: .f90 .o .mod


.f90.o:
	$(F90_COMPILE) $*.f90
.f90.mod:
	$(F90_COMPILE) $*.f90

EXECUTABLES = Life
OBJS = Life.o\
       gif.o\
       gif_util.o\
       m_mrgref.o

all: $(EXECUTABLES)

Life: $(OBJS)
	$(F90_LOAD) $(OBJS) -o Life

Life.o: gif_util.o gif.o m_mrgref.o

clean:
	rm -f *.o *.mod *.d $(EXECUTABLES)

