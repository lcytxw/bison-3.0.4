#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include "print.h"
#include <stdlib.h>

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size){
        if(size < 1 )return 0;
//	FILE *pFile = fopen("test.txt","rw");
	char tmp[] = "template-XXXXXX";
	int fd = mkstemp(tmp);
	if (fd < 0) {
		printf("fd < 0");
		return 0;
	}
	write(fd, data, size);
//      char *str = new char[size+1];
//      memcpy(str, data, size);
//      str[size] = 0;
        int argc0=2;
        char *argv0[2];
        argv0[0] = (char *)"bison";
        argv0[1] = tmp;
        fuzz_main(argc0, argv0);
	close(fd);
        return 0;
}
