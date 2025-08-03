/* Compute exponential series, author: Albert Mink */
import java.util.Scanner;
public class Expo{
  public static void main(String[] args) {
    Scanner in = new Scanner(System.in);
    System.out.println("Berechne Exponentialfunktion exp(x) fuer x = ");
    double x = in.nextDouble();
    double exp = 1;
    double exp_alt = 0;
    double yi = 1;
    int i = 1;
    do {
      exp_alt = exp;
      yi = x/i *yi;
      exp = exp_alt + yi;
      i++;
    } while ( Math.abs(exp-exp_alt) >= 1e-12 );
    System.out.println("exp(x) = " + exp);
    System.out.println("Abbruch nach " +i+ " Schleifendurchlaeufen");
    System.out.println("Differenz " + (Math.exp(x)-exp));
  }
}
