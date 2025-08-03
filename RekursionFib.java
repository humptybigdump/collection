import java.util.*;
public class RekursionFib {
  // function computes fib rec
  static int i = 0;
  public static int fibonacci(int n) {
    i += 1;
    if ( n <= 1 ) {
      return 1;
    } else {
      return fibonacci(n-1) + fibonacci(n-2);
    }
  }

  public static void main(String[] args) {
    System.out.println(fibonacci(4));
    System.out.println("#Aufrufe :"+i);
    i = 0;
    System.out.println(fibonacci(8));
    System.out.println("#Aufrufe :"+i);
  }
}
