#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>
#include <inttypes.h>
#include <stdbool.h>

#include <avr/io.h>

static void PutChar(char c)
{
   *((volatile uint8_t*)0x20) = c; // Output register
}

static void PutStr(const char* str)
{
   char c;
   while(c = *str++)
      PutChar(c);
}

int main(void)
{
   PutStr("Hello world\n");
   
   *((volatile uint8_t*)0x21) = 123; // Exit code
   *((volatile uint8_t*)0x22) = 1; // Exit request set
   
   while(1);
}