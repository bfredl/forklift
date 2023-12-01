#include <stdbool.h>
#include <stdio.h>

int doit(char *data, int len, char *table, int tablelen) {
  int x, first, current, sum, y, tpos, i, imatch, scan;
  x = 0;
  sum = 0;
  while (true) {
    if (x >= len) break;
    first = 10;
    current = 0; // dummy init
    while (true) {
      if (x >= len) break;
      int byteval = data[x];
      int trydigit = byteval - 48;
      if (trydigit <= 9 && trydigit >= 0) {
        current = trydigit;
        if (first >= 10) {
          first = trydigit;
        }
      } else {
        tpos = 0;
        i = 0;
        while (true) {
          scan = x;
          imatch = 1;
          while (true) {
            int tval = table[tpos];
            tpos = tpos + 1;
            if (tval == 10) break;
            if (scan >= len) break;
            if (tval != data[scan]) {
              imatch = 0;
            }
            scan = scan + 1;
          }
          if (imatch == 1) {
            current = i;
            if (first >= 10) {
              first = i;
            }
          }
          if (imatch == 1) break; // BULL
          // if (tpos >= tablelen) break;
          if (table[tpos] == 0) break;
          i = i + 1;
        }
      }
      x = x + 1;
      if (byteval == 10) break;
    }
    int item = 10*first+current;
    sum = sum + item;
  }
  return sum;
}

int main(int argc, char **argv) {
  static char ungodly[100000];
  static char cursed[100000];
  FILE *fil1 = fopen(argv[1], "r");
  FILE *fil2 = fopen(argv[2], "r");
  int len1 = fread(ungodly, 1, 100000, fil1);
  int len2 = fread(cursed, 1, 100000, fil2);
  int res = doit(ungodly, len1, cursed, len2);
  printf("res %d\n", res);
}
