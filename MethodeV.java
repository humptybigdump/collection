// Author: A. Mink, Sept.2019
public class MethodeV{
  // Methode ohne Rueckgabewert!
  static void addOne(int n) {
    int m = n+1;
    Systen.out.println(n+"+1="+m);
  }
  public static void main (String [] args){
    int a = 2;
    addone(a); // prints 2+1=3
    addOne(2); // prints 2+1=3 (alt.)
  }
}
