OBJS = main.o parser.o tree.o

all: ../tiny

clean:
	rm *.o parser.cpp scanner.cpp

parser.cpp: parser.y
	yacc --yacc -oparser.cpp parser.y

scanner.cpp: scanner.l
	lex -l -oscanner.cpp scanner.l

parser.o: parser.cpp scanner.cpp

../tiny: $(OBJS)
	g++ -o ../tiny $(OBJS)
