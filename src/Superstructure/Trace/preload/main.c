#include <stdio.h>
#include <unistd.h>

int main()
{
    write(0, "Hello, Kernel!\n", 15);
    printf("Hello, World!\n");

    return 0;
}
