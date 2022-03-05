COMPILER = gfortran
FLAGS = -Wall -Wextra -Wconversion \
		-O0 -fdefault-real-16 -freal-8-real-16 \
		-fdefault-integer-8 -g \
		-finit-real=nan -fcheck=all

SRC=src
OBJ=obj
BIN=bin

EXEC = $(BIN)/main
OBJS = Constants.o Logging.o Random.o IO.o Testing.o Utils.o Assert.o Distr.o

all: $(SRC)/main.f90 $(OBJS)
	$(COMPILER) $(FLAGS) -o $(EXEC) $(SRC)/main.f90 $(OBJS)
	
clean:
	rm -rf $(BIN)/main $(BIN)/main.dSYM *.mod *.o *.gcno *.gcda *.info

Assert.o: $(SRC)/Assert.f90 Constants.o
	$(COMPILER) $(FLAGS) -c $(SRC)/Assert.f90

Constants.o: $(SRC)/Constants.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Constants.f90

Distr.o: $(SRC)/Distr.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Distr.f90

Logging.o: $(SRC)/Logging.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Logging.f90

IO.o: $(SRC)/IO.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/IO.f90

Random.o: $(SRC)/Random.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Random.f90

Utils.o: $(SRC)/Utils.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Utils.f90

Testing.o: $(SRC)/Testing.f90
	$(COMPILER) $(FLAGS) -c $(SRC)/Testing.f90