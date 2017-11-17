#include <string.h>
#include <stdint.h>
#include "print.h"
/*
extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size){
	if(size < 1 )return 0;
	char *str = new char[size+1];
	memcpy(str, data, size);
	str[size] = 0;
	int argc=2;
	char *argv[2];
	argv[0] = (char *)"bison";
	argv[1] = str;
	int a = fuzz_main(argc, argv);
	return 0;
}
*/
int main(){
	int argc = 2;
	char *argv[2];
        argv[0] = (char *)"bison";
        argv[1] = (char *)"abc";
        int a = fuzz_main(argc, argv);
}
