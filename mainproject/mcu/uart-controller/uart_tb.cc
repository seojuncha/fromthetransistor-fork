#include <fcntl.h>
#include <unistd.h>
#include <iostream>

#include "obj_dir/Vuart.h"
#include "verilated.h"

/**
 * How to test
 * 1. socat -d -d pty,raw,echo=0 pty,raw,echo=0
 * 2. run this testbench
 * 3. screen /dev/ttys009 9600
 * 4. Enter characters in screen
 * 5. Exit the screen program
 */

int main(int argc, char** argv)
{
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);
  Vuart* top = new Vuart{contextp};

  // 
  int uart_fd = open("/dev/ttys010", O_RDWR | O_NOCTTY);
  if (uart_fd < 0) {
    fprintf(stderr, "invalid uart_fd\n");
    return 1;
  }

  printf("Wait reading...\n");
  char rx_byte;
  while (read(uart_fd, &rx_byte, 1) > 0) {
    printf("%c", rx_byte);
  }

  printf("DONE\n");

  close(uart_fd);

  delete top;
  delete contextp;
  return 0;
}