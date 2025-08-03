/* Illustration of arrays
   Author: Albert Mink, Nov 2017 */
import java.util.Arrays;
public class Felderc {
  public static void main (String[] args) {
    int[] feld = {5, 4, 3, 2, 1};
    for (int i = 0; i < feld.length; i++) {
      feld[i] = feld[feld.length-i-1];
    }
    System.out.println(Arrays.toString(feld));
  }
}
