#include <stdio.h>
#include <stdbool.h>
#include <string.h>

int main(void)
{
  printf("%a / %a = %a\n", 1.0001, 1.0002, 1.0001/1.0002);
  // prints:
  // 0x1.00068db8bac71p+0 / 0x1.000d1b71758e2p+0 = 0x1.fff2e53a4e1dap-1

  // Building the problem:
  // We are interested in the equation:
  // X / [0x1.000d1b71758e2p+0 … 2.0] == 0x1.fff2e53a4e1dbp-1

  printf("%a / %a = %a\n", 0x1.00068db8bac72p+0, 0x1.000d1b71758e2p+0, 0x1.00068db8bac72p+0/ 0x1.000d1b71758e2p+0);
  // This makes 0x1.fff2e53a4e1dcp-1: too high

  printf("%a / %a = %a\n", 0x1.00068db8bac72p+0, 0x1.000d1b71758e3p+0, 0x1.00068db8bac72p+0/ 0x1.000d1b71758e3p+0);
  // This makes 0x1.fff2e53a4e1dap-1: too low

  printf("%a / %a = %a\n", 0x1.00068db8bac73p+0, 0x1.000d1b71758e3p+0, 0x1.00068db8bac73p+0/ 0x1.000d1b71758e3p+0);
  // This makes 0x1.fff2e53a4e1dcp-1: too high again

  printf("%a / %a = %a\n", 0x1.00068db8bac73p+0, 0x1.000d1b71758e4p+0, 0x1.00068db8bac73p+0/ 0x1.000d1b71758e4p+0);
  // This makes 0x1.fff2e53a4e1dap-1: too low again

}
