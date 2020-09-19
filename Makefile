FC=gfortran-mp-5

FFLAGS = -O3

LNK=gfortran

OBJS = main.o predict.o train.o read_data.o grad.o check_parameters.o 

MODS = global_numbers.o arrays.o

$(OBJS):	$(MODS)

svm:  $(OBJS) $(MODS)
		$(LNK) $(FFLAGS) -o xsvm $(OBJS) $(MODS) 
	@ mkdir -p xxx
	@ mv xsvm xxx

.PHONY:	clean

clean:
	-rm -f *.o *.mod xxx/xsvm xxx/predict.dat xxx/train.dat

%.o : %.f90
	$(FC) -c $(FFLAGS) $< -o $@

