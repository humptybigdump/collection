/* Minimal example for functions*/
import java.util.*;
public class Funktionen {
  public static void foo() {
    System.out.println("Hello World!");
  }
  public static int addOne(int a) {
    return a + 1;
  }
  public static double add(double lhs, double rhs) {
    return lhs + rhs;
  }
  public static void main(String[] args) {
    foo();
    System.out.println("addOne(3): " + addOne(3));
    System.out.println("add(1.1,7): "+add(1.1,7));
  }
}
