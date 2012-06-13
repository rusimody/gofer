# Two ways to compile the expert system, one with gofer and one with hbc:

expert: Result.hs Table.hs Knowledge.hs Match.hs Search.hs Main.hs
	goferc + expert.prj

expert2: Result.o Table.o Knowledge.o Match.o Search.o Main.o
	hbc -o expert2 *.o

Result.o: Result.hs
	hbc -c Result.hs

Table.o: Table.hs Result.o
	hbc -c Table.hs

Knowledge.o: Knowledge.hs Table.o Result.o
	hbc -c Knowledge.hs

Match.o: Match.hs Knowledge.o Table.o Result.o
	hbc -c Match.hs

Search.o: Search.hs Match.o Knowledge.o Table.o Result.o
	hbc -c Search.hs

Main.o: Main.hs Search.o Match.o Knowledge.o Table.o Result.o
	hbc -c Main.hs
