// Author: A. Mink, Nov.2017, side effects
public class FelderFunktionen{
  static double computeNorm(int n) {
    n = n*n;
    return Math.sqrt(n);
  }
  // Ueberladung, overload
  static double computeNorm(int[] x) {
    x[0] = x[0]*x[0] + x[1]*x[1];
    return Math.sqrt(x[0]);
  }
  public static void main (String [] args){
    int a = 2;
    System.out.println(computeNorm(a));
    System.out.println(a);

    int[] d = {-1, 1};
    System.out.println(computeNorm(d));
    System.out.println(d[0]);    // What happened?
  }
}
