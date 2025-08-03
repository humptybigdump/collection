/* Illustration of arrays
   Author: Albert Mink, Nov 2017 */
import java.util.Arrays;
public class Felderf {
  public static void main (String[] args) {
    int[] feld = {5, 4, 3, 2, 1};
    for (int i = feld.length-1; i >= 0; i--) {
      int k = feld[i];
      feld[i] = feld[feld.length-i-1];
      feld[feld.length-i-1] = k;
    }
    System.out.println(Arrays.toString(feld));
  }
}
