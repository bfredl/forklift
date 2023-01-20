#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>


typedef intptr_t isize;
isize doit(char *data, isize len);
int main(int argc, char **argv) {
  FILE *input = fopen(argv[1], "rb");
  fseek(input, 0, SEEK_END);
  isize len = ftell(input);
  fseek(input, 0, SEEK_SET);
  char *buf = alloca(len+1);
  if(!fread(buf, len, 1, input)) return 5;
  buf[len] = '\0';
  isize res = doit(buf, len);
  printf("res: %ld\n", res);
}
