#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "intrinsic.h"

// Run with ikos using "--widening-delay 10" 

void error(const char *msg) {
  printf("%s\n", msg);
  exit(1);
}

int eq(const char *s1, const char *s2, int n) {
  for(int i = 0; i < n; i++) {
    if(s1[i] != s2[i]) return 0;
  }
  return 1;
}

void readline(char *buffer, int n) {
  int i = 0;
  while(i < n) {
#ifdef __IKOS__
    char c = __ikos_nondet_int();
#else
    char c = getchar();
#endif
    if(c == '\n') {
      break;
    }
    buffer[i] = c;
    i++;
  }
  buffer[i] = 0;
}

void loop(char* buffer)
{
  
  int stack[10];
  int count = 0;

  while(1) {
    printf("> ");
    readline(buffer, 10);

    if(eq(buffer, "+", 2)) {
      if(count < 2) {
        error("missing operands");
      } else {
        int a = stack[count-2];
        int b = stack[count-1];
        long sum = (long)a + (long)b;
        if(sum > INT_MAX || sum < INT_MIN) error("overflow");
        stack[count-2] = sum;
        count--;
      }
    }

    else if(eq(buffer, "-", 2)) {
      if(count < 2) error("missing operands");
      int a = stack[count-2];
      int b = stack[count-1];
      long sum = (long)a - (long)b;
      if(sum > INT_MAX || sum < INT_MIN) error("overflow");
      stack[count-2] = sum;
      count--;
    }

    else if(eq(buffer, "*", 2)) {
      if(count < 2) error("missing operands");
      int a = stack[count-2];
      int b = stack[count-1];
      long sum = (long)a * (long)b;
      if(sum > INT_MAX || sum < INT_MIN) error("overflow");
      stack[count-2] = sum;
      count--;
    }

    else if(eq(buffer, "q", 1)) {
      return;
    }

    else {
      if(count == 10) {
        error("Buffer full");
      } else {
    
        int i = 0;

        int factor;
        if(eq(buffer, "-", 1)) {
          factor = -1;
          i++;
        } else {
          factor = 1;
        }

        int value = 0; // rem
        while(1) {
          char digit = buffer[i];
          if('0' > digit || digit > '9') break;
          if(i > 9) break;
          if(value >= (1<<27)) error("value too large");
          
          value = value * 10 + digit - '0';
          i = i + 1;
        }
    
        if(buffer[i] != 0) error("Not a number");

        if(factor == -1) stack[count] = -value;
        else stack[count] = value;

        count ++;
      }
    }

    for(int i = 0; i < count; i++) { // <=
      printf("[%d] = %d\n", i, stack[i]);
    }
  
  }
}
int main() {
  char buffer[] = "12345678901";  // to 8
  // char *buffer = "b";
  loop(buffer);
}

