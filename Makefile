COMPILER = gfortran
FLAGS = -Wall -Wextra -Wconversion \
		-O2 -fdefault-real-16 -freal-8-real-16 \
		-fdefault-integer-8 -g \
		-finit-real=nan -fsanitize=undefined -fcheck=all

SRC=src
OBJ=obj
BIN=bin

EXEC = $(BIN)/main
OBJS = Assert.o Commons.o Constants.o \
	   Distributions.o IO.o Interpolation.o \
	   LinearAlgebra.o \
	   Plots.o  Polynomials.o \
	   Random.o RootFinding.o Testing.o

all: $(SRC)/main.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) $(SRC)/main.f90 $(OBJS)

plot: $(SRC)/main_plot.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) $(SRC)/main_plot.f90 $(OBJS)

random: $(SRC)/main_random.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) $(SRC)/main_random.f90 $(OBJS)

library: $(OBJS)
	$(COMPILER) $(FLAGS) -shared -o NumericalFortran.dylib $(OBJS)

clean:
	rm -rf $(BIN)/main $(BIN)/main.dSYM *.mod *.o *.gcno *.gcda *.info

Assert.o: $(SRC)/Assert.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Assert.f90

Commons.o: $(SRC)/Commons.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Commons.f90

Constants.o: $(SRC)/Constants.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Constants.f90

Interpolation.o : $(SRC)/Interpolation.f90 Constants.o LinearAlgebra.o IO.o Polynomials.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Interpolation.f90

IO.o: $(SRC)/IO.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/IO.f90

LinearAlgebra.o: $(SRC)/LinearAlgebra.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/LinearAlgebra.f90

Distributions.o: $(SRC)/Distributions.f90 Commons.o Constants.o Random.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Distributions.f90

Plots.o: $(SRC)/Plots.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Plots.f90

Polynomials.o: $(SRC)/Polynomials.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Polynomials.f90

Random.o: $(SRC)/Random.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Random.f90

RootFinding.o: $(SRC)/RootFinding.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/RootFinding.f90

Testing.o: $(SRC)/Testing.f90 Assert.o Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Testing.f90