/* Illustration of arrays
   Author: Albert Mink, Nov 2017 */
import java.util.Arrays;
public class Felderb {
  public static void main (String[] args) {
    int[] feld = {1, 4, 9, 16, 25};
    for (int i = feld.length-1; i > 0; i--) {
      feld[i] -= feld[i-1];
    }
    System.out.println(Arrays.toString(feld));
  }
}
