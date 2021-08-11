all:root
root:ass2s.o
	gcc -g -Wall  -o root ass2s.o
ass2s.o:ass2s.s
	nasm -g -f elf64 -w+all -o ass2s.o ass2s.s
.PHONY:clean
clean:
	rm -f *.o root
