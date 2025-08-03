// compute integral via trapezoidal rule
import java.util.*;
public class Trapez{
  public static double f(double x) {
    return -x*x + 4;
  }
  public static void main(String[] args) {
    double a = 0;
    double b = 3;
    int N = 6;
    // (a)
    double I1 = (b-a) * (f(a) + f(b)) / 2;
    // (b)
    double sum = 0;
    for (int n = 1; n < N; n++) {
      sum += f(a + n * (b-a)/N);
    }
    double I2 = (b-a)/N * (0.5*f(a) + 0.5*f(b) + sum);
    System.out.println("a: " + I1);
    System.out.println("b: " + I2);
  }
}
