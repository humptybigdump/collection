/* Illustration if-then-else Statements
   Author: Albert Mink, 03.Nov 2017 */
public class Anweis2 {
  public static void main (String[] args) {
    int i = -1;
    if ( i < 1 || i > 3 ) {
      System.out.println("A");
    } else if ( i == 1 || i == 2 ) {
      System.out.println("B");
    } else {
      System.out.println("C");
    }
  }
}
