#include <stdbool.h>
#include <stdio.h>

int doit(char *data, int len) {
int row, off, coloff, summa, stride, curany, lastany, nextany, seensymb, value;
  stride = 1;
  while(true) {
    if (data[stride-1] == 10) break;
    stride = stride + 1;
  }
  printf("S %d\n", stride);


  summa = 0;
  row = 0;
  off = 0;
  while(true) {
    //col = 0;
    coloff = off;

    while(true) {
      //int coloff = off+col;
      int first = data[coloff] - 48;
      if (first <= 9 && first >= 0) {
        seensymb = 0;
        value = 0;
        if (coloff > off) { // col > 0
          seensymb = seensymb | !!(data[coloff-1]-46);
          if (row > 0) {
            seensymb = seensymb | !!(data[coloff-stride-1]-46);
          }
          if (stride + off < len) {
            seensymb = seensymb | !!(data[coloff+stride-1]-46);
          }
        }
        // col = col + 1;
        // coloff = coloff + 1;
        while(true) {
          if (coloff >= off+stride-1) break;
          // unconditionally consider above and below
          if (row > 0) {
            seensymb = seensymb | !!(data[coloff-stride]-46);
          }
          if (coloff + stride < len) {
            seensymb = seensymb | !!(data[coloff+stride]-46);
          }
          int next = data[coloff] - 48;
          if (next <= 9 && next >= 0) {
            value = value*10+next;
          } else {
            // consider symbol right to the right of number;
            seensymb = seensymb | !!(data[coloff]-46);
            break;
          }
          coloff = coloff + 1;
        }
        if (seensymb != 0) { // ANY
          printf("begå %d\n", value);
          summa = summa + value;
        }
      }
      coloff = coloff + 1; // can unconditionally skip one more
      if (coloff >= off+stride-1) break;
    }

    off = off + stride;
    row = row + 1;
    if (off >= len) break;
  }
  return summa;
}

int main(int argc, char **argv) {
  static char ungodly[100000];
  FILE *fil1 = fopen(argv[1], "r");
  int len1 = fread(ungodly, 1, 100000, fil1);
  int res = doit(ungodly, len1);
  printf("res %d\n", res);
}
