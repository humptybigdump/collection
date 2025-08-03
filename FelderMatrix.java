// Author: A. Mink, M. Wu, multidimensional arrays
import java.util.Arrays;
public class FelderMatrix{
  public static double[][] getIdentity() {
    // row major, [m][n] -> m-row and n-column
    return new double[][]{{1,0,0},{0,1,0},{0,0,1}};
  }
  public static double[] getDiagEl(double[][] A) {
	return new double[]{A[0][0],A[1][1],A[2][2]};
  }  
  public static double getTrace(double[][] A) {
    return A[0][0]+A[1][1]+A[2][2];
  }
  public static void printMatrix(double[][] A) {
    for ( int m = 0; m < A.length; m++ ) {
      System.out.println(Arrays.toString(A[m]));
    }
  }
  public static double getMaxValue(double[][] A) {
    double max = 0;
    for ( int m = 0; m < A.length; m++ ) {
      for ( int n = 0; n < A[m].length; n++ ) {
        if ( A[m][n] > max ) {
          max = A[m][n];
        }
      }
    }
    return max;
  }
  public static double getSpaltensummennorm(double[][] A) {
    double max = 0;
    for ( int n = 0; n < A[0].length; n++ ) {
      double[] spalte = new double[A[0].length];
      for ( int m = 0; m < A.length; m++ ) {
        spalte[m] = A[m][n];
		//nehmen das n-te Element von jede Zeile = n-te Spalte
      }
      double spaltenNorm = 0;
      for ( int i = 0; i < A.length; i++ ) {
        spaltenNorm += Math.abs(spalte[i]);
      }
      if (spaltenNorm>max) {
        max = spaltenNorm;
      }
    }
    return max;
  }

  public static void main (String [] args){
    double[][] E = getIdentity();
    printMatrix(E);
    System.out.println("\nMax Value " + getMaxValue(E));
	System.out.println("Diagonalelemente " + Arrays.toString(getDiagEl(E)));
    System.out.println("Spur " + getTrace(E));
    System.out.println("Spaltensummennorm " + getSpaltensummennorm(E));
  }
}
