COMPILER = gfortran
FLAGS = -O2
EXEC = bin/main

OBJS = Constants.o

all: src/main.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) src/main.f90 $(OBJS)
	
Constants.o: src/Constants.f90
	$(COMPILER) $(FLAGS) -c src/Constants.f90
	
plot: src/main_plot.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) src/main_plot.f90 $(OBJS)

clean:
	rm -rf bin/main bin/main.dSYM *.mod *.o
