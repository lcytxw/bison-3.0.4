=================================================
=================================================
makefile已经更改过，不要重新configure，否则无法成功。

将原来的main函数放到了print.c中。

COV_FLAGS="-fsanitize-coverage=trace-pc-guard"

clang -g -fsanitize=address $COV_FLAGS -c -I ./ myfuzz.c

clang++ -std=c++11  -fsanitize=address -fsanitize-coverage=trace-pc-guard -g myfuzz.o /home/icy/comp/libFuzzer/libFuzzer.a  libbison.a  -I ./  -o myfuzzer 
