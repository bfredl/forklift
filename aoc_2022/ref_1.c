#include <stdint.h>
#include <stdbool.h>

typedef intptr_t isize;

isize doit(char *data, isize len) {
  isize sum, item, ipos, max;
  max = 0;
  ipos = 0;
  while (ipos < len) { // fyy
    sum = 0;
    while (ipos < len) {
      item = 0;
      const isize prebyte = data[ipos];
      const isize pretoken = prebyte-48;
      if (pretoken < 0 || pretoken > 9) break;
      while (ipos < len) { // fyyy
        const isize byte = data[ipos];
        const isize token = byte-48;
        if (token < 0 || token > 9) break;
        item = 10*item+token;
        ipos++;
      }
      sum += item;
      ipos++;
    }
    ipos++;
    if (sum > max) max = sum;
  }
  return max;
}
