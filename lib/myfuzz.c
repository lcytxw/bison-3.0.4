#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include "print.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
int fd, res;
int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size){
        if(size < 1 )return 0;
	if((fd = open("tmpfile",O_RDWR|O_CREAT|O_TRUNC)) < 0){
		perror("Open file faild");
		return 1;
	}
//	FILE *pFile = fopen("test.txt","rw");
	if((res = write(fd, data, size)) != size) {
		perror("Write to file faild");	
	}
//      char *str = new char[size+1];
//      memcpy(str, data, size);
//      str[size] = 0;
        int argc0=2;
        char *argv0[2];
        argv0[0] = (char *)"bison";
        argv0[1] = (char *)"tmpfile";
        fuzz_main(argc0, argv0);
	close(fd);
        return 0;
}
