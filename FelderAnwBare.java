// Author: A. Mink, Nov.2017
import java.util.Scanner;
import java.util.Arrays;
//or import java.util.*;

public class FelderAnwBare {
  public static void main(String[] args) {
    Scanner in = new Scanner(System.in);
    System.out.println("Gebe Vektor an (n=3): ");
    // get values from terminal
    double[] x = new double[3];

    // compute norm^2 of vector
    double norm2 = 0;

    System.out.println(Arrays.toString(x));
    System.out.println("Norm: " + Math.sqrt(norm2));
  }
}
