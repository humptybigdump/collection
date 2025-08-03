// Author: A. Mink, Nov.2017
import java.util.Scanner;
import java.util.Arrays;
public class FelderAnw {
  public static void main(String[] args) {
    Scanner in = new Scanner(System.in);
    System.out.println("Gebe Vektor an (n=3): ");
    // get values from terminal
    double[] x = new double[3];
    for ( int i = 0; i<x.length; i++ ) {
      x[i] = in.nextDouble();
    }
    // compute norm^2 of vector
    double norm2 = 0;
    for ( int i = 0; i<x.length; i++) {
      norm2 = norm2 + x[i]*x[i];
    }
    System.out.println(Arrays.toString(x));
    System.out.println("Norm: " + Math.sqrt(norm2));
  }
}
