// Illustration of loops, Author: Albert Mink 11.2017
import java.util.Arrays;
public class FelderGrundlagen {
  public static void main (String[] args) {
    int[] feld = new int[10];
    for(int i = 0; i < feld.length; i++) {
      feld[i] = 1;
      for(int j = 0; j < i; j++) {
        feld[i] = feld[i] + feld[j];
      }
    }
    System.out.println(Arrays.toString(feld));
  }
}
