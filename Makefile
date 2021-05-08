COMPILER = gfortran
FLAGS = -Wall -Wextra -Wconversion -O2 -fdefault-real-16 -freal-8-real-16 -fdefault-integer-8

SRC=src
OBJ=obj
BIN=bin

EXEC = $(BIN)/main
OBJS = Assert.o Commons.o Constants.o \
	   Distributions.o IO.o Plots.o  \
	   Random.o Testing.o

all: $(SRC)/main.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) $(SRC)/main.f90 $(OBJS)

plot: $(SRC)/main_plot.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) $(SRC)/main_plot.f90 $(OBJS)

random: $(SRC)/main_random.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) $(SRC)/main_random.f90 $(OBJS)

clean:
	rm -rf $(BIN)/main $(BIN)/main.dSYM *.mod *.o

Assert.o: $(SRC)/Assert.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Assert.f90

Commons.o: $(SRC)/Commons.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Commons.f90

Constants.o: $(SRC)/Constants.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Constants.f90

IO.o: $(SRC)/IO.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/IO.f90

Distributions.o: $(SRC)/Distributions.f90 Commons.o Constants.o Random.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Distributions.f90

Plots.o: $(SRC)/Plots.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Plots.f90

Random.o: $(SRC)/Random.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Random.f90

Testing.o: $(SRC)/Testing.f90 Assert.o Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Testing.f90