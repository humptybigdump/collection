/* Illustration of arrays
   Author: Albert Mink, Nov 2017 */
import java.util.Arrays;
public class Feldere {
  public static void main (String[] args) {
    int[] feld = {1, 2, 3, 4, 5};
    for (int i = 0; i < feld.length; i++) {
      feld[i] *= (i+1);
    }
    System.out.println(Arrays.toString(feld));
  }
}
