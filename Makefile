OBJS	=	\
file1.o	\
file2.o

ABC = 
CC	=	gcc
CFLAGS	=	-Wall	-O	-g

helloworld	:	$(OBJS)
	$(CC)	$(OBJS)	-o	helloworld

file1.o	:	file1.c	file2.h
	$(CC)	$(CFLAGS)	-c	file1.c	-o	file1.o

file2.o	:	\
file2.c	\
file2.h
	$(CC)	$(CFLAGS)	-c	file2.c	-o	file2.o
	$(CC)	$(CFLAGS)	-c	file2.c	-o	file2.o

file3.o :
	gcc \
	-v

clean:
	rm	-rf	\*.o	

