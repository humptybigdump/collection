/* Minimal example for call-by-value*/
import java.util.*;
public class Funktionen_bem {
  private static int beispiel() {
	  return 0;
  }
	
  public static int returnOne(int a) {
    a = 42; // Variable a ist lokale Kopie, hier von i=3
	beispiel();
    return 1;
  }
  public static void main(String[] args) {
    int i = 3;
    System.out.println(returnOne(i));
    System.out.println(i);
  }
}
