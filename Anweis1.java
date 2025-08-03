/* Illustration if-then-else Statements
   Author: Albert Mink, 03.Nov 2017 */
import java.util.Scanner;
public class Anweis1 {
  public static void main (String[] args) {
    Scanner in = new Scanner(System.in);
    System.out.print("Gebe Variable int n an: ");
    int n = in.nextInt();
    if ( n < 3 ) {
      if ( n > 1 ) {
        System.out.println("A");
      } else {
        System.out.println("B");
      }
    } else {
      if ( n <= 3 ) {
        System.out.println("C");
      } else {
        System.out.println("D");
      }
    }
  }
}
