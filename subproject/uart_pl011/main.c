#include <stdio.h>

#include "uart.h"

/**
 * ref: https://github.com/YWHyuk/WAIOS/tree/uart
*/
void main(void)
{
  char buffer[] = "abcd";
  init_uart();
  for (int i = 0; i < 13; i++) {
    write_uart(buffer[i]);
  }
  while (1) {
    write_uart(read_uart());
  }
}