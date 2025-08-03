/* Illustration of arrays
   Author: Albert Mink, Nov 2017 */
import java.util.Arrays;
public class Felderd {
  public static void main (String[] args) {
    int[] feld = {1, 0, 4, 2, 3};
    for (int i = 0; i < feld.length; i++) {
      feld[i] = feld[feld[i]];
    }
    System.out.println(Arrays.toString(feld));
  }
}
