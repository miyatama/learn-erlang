#include <stdio.h>
#include "example1.h" 

typedef unsigned char byte;

int read_cmd(byte *buff);
int write_cmd(byte *buff, int len);

int main() {
  int fn, arg1, arg2, result;
  byte buff[100];
  while (read_cmd(buff) > 0){
    fn = buff[0];
    if (fn == 1) {
      arg1 = buff[1];
      result = twice(arg1);
    } else if (fn == 2) {
      arg1 = buff[1];
      arg2 = buff[2];
      result = sum(arg1, arg2);
    }
    buff[0] = result;
    write_cmd(buff, 1);
  }
}
