COMPILER = gfortran
FLAGS = -Wall -Wextra -Wconversion -O2 -fdefault-real-8

SRC=src
OBJ=obj
BIN=bin

EXEC = $(BIN)/main
OBJS = Assert.o Constants.o Plots.o Random.o Testing.o

all: $(SRC)/main.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) $(SRC)/main.f90 $(OBJS)

plot: $(SRC)/main_plot.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) $(SRC)/main_plot.f90 $(OBJS)

clean:
	rm -rf $(BIN)/main $(BIN)/main.dSYM *.mod *.o

Assert.o: $(SRC)/Assert.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Assert.f90
	
Constants.o: $(SRC)/Constants.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Constants.f90
	
Plots.o: $(SRC)/Plots.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Plots.f90

Random.o: $(SRC)/Random.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Random.f90

Testing.o: $(SRC)/Testing.f90 Assert.o Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Testing.f90