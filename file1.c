#include "stdio.h"
#include "stdlib.h"

#include "file1.h"
#include "file2.h"

void file1func() 
{
   printf("file1func\n");
}

int main() {
   file1func();
   file2func();

   return 0;
}

