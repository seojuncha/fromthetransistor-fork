#include <stdio.h>

int main(void)
{
    char send_char = 'a';
    char recv_char;

    printf("Sending... %c\n", send_char);

    putchar(send_char);

    printf("\nSent and wait..\n");

    // recv_char = getchar();
    // printf("Received : %c\n", recv_char);

    return 0;
}
