// Author: A. Mink, Sept.2019
public class Methode{
  // Datentyp von Rueckgabewert integer
  static int addOne(int n) {
    int m = n+1;
    return m; // gebe Variable m zurueck
  }
  public static void main (String [] args){
    int a = 2;
    int ergebnis = addOne(a);
    System.out.println(ergebnis);  // prints 3
    System.out.println(addOne(2)); // prints 3 (alt.)
  }
}
