#include <stdio.h>

extern int blungentle_main() asm("blungentle_main");

int main(int argc, char** argv) {
  int result = blungentle_main();
  printf("%d\n", result);
  return 0;
}

int read_int() {
    int i;
    scanf("%d", &i);
    return i;
}
