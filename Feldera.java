/* Illustration of arrays
   Author: Albert Mink, Nov 2017 */
import java.util.Arrays;
public class Feldera {
  public static void main (String[] args) {
    int[] feld = {1, 2, 3, 4, 5};
    for (int i = 1; i < feld.length; i++) {
      feld[i] += feld[i-1];
    }
    System.out.println(Arrays.toString(feld));
  }
}
