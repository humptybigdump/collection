import java.util.Arrays;
public class Effizienz{
  public static void main (String[] args) {
    int n = 2;
    double[][] a = {{1,2},{3,4}};
    double[] b = {5,6};
    double[] c = {7,8};
    double[] res = new double[n];
    for (int mu = 0; mu < n; mu++) {
      double skp = 0.0;
      for (int nu = 0; nu < n; nu++) {
        skp += a[mu][nu] * b[nu];
      }
      res[mu] = skp + c[mu];
    }
    System.out.println(Arrays.toString(res));
  }
}
