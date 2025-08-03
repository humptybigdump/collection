import java.util.*;
public class Vorwaerts {
  public static double[] vore(double[][] A, double[] b) {
    double[] y = new double[b.length];
    for (int i = 0; i < b.length; i++) {
      double sum = 0;
      for (int k = 0; k < i; k++) {
        sum += A[i][k] * y[k];
      }
      y[i] = 1./A[i][i] * (b[i] - sum);
    }
    return y;
  }

  public static void main(String[] args) {
    double[][] A = {{1.,0.,0.},{0.5,1.,0.},{0,2./3,1.}};
    double[] b = {10.,9.,6.};
    System.out.println(Arrays.toString(vore(A, b)));
  }
}
