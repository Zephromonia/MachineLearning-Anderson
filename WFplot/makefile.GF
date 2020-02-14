#-----------------------------------------------------------------
#
# tmse2dCOE
#
#-----------------------------------------------------------------
#
# $Header: /home/cvs/phsht/WFplot/makefile.GF,v 1.1 2019/09/18 09:56:03 phsht Exp $
#
# $Log: makefile.GF,v $
# Revision 1.1  2019/09/18 09:56:03  phsht
# added maqkefile.GF to work with standard gfortran;
# added FRAME input;
# switched COLOR to 1 and B/W to 0
#
# Revision 1.1  2008/06/13 16:47:46  phsht
# from tmse2dXCOE
#
#-----------------------------------------------------------------#

F77 = gfortran -ffixed-form -std=legacy
F90 = gfortran
#F90 = gfortran

#F77FLAGS = # For debugging
F77FLAGS = -fbounds-check -fno-range-check -Warray-bounds  # For optimizing
#F77FLAGS = # For profiling

#F90FLAGS = -g -std=legacy -fbounds-check -fno-range-check -fimplicit-none -Warray-bounds #-Wall # For debugging
F90FLAGS = -ffree-line-length-none -fbounds-check -fno-range-check -fimplicit-none -fcray-pointer -fexternal-blas # For optimizing
#F90FLAGS = -fno-range-check # for profling

CC = gcc
CCFLAGS = -O2

LIBDIR = $(HOME)/f77/lib
LIBFLAGS  =  -llapack -lblas

PRINT1 = a2ps -g -o-
PRINT2 = > mcXdCOE.ps # | fleet -odouble

WFplot.GF:    main.o util.o 
	$(F90) $(F90FLAGS) -o WFplot.GF \
		util.o main.o $(LIBFLAGS)

main.o:     main.f90
	$(F90) $(F90FLAGS) -c main.f90

util.o:     util.f90
	$(F90) $(F90FLAGS) -c util.f90

tar:		WFplot.tar
WFplot.tar:	makefile.* main.f90 util.f90 
	tar -cvf WFplot.tar makefile.* main.f90 util.f90 

print:  
	$(PRINT1) *.f90 $(PRINT2)

clean:
	rm -f core *.mod *.o




