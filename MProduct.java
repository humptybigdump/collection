import java.util.Scanner;

public class MProduct {

    public static void printMatrix(int[][] a) {
        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < a[i].length; j++) {
                System.out.print(a[i][j] + " ");
            }
            System.out.println();
        }
    }


    public static void main(String[] args) {
        // use a Scanner to ask for format from stdin stream
        Scanner scan = new Scanner(System.in);
        // be nice
        System.out.print("Desired matrix format nxn, n = ");
        int n = scan.nextInt();
        if (n < 0) {
            System.out.println("n has to be positive");
            return;
        }
        if (n == 0) {
            System.out.println("Powers of empty matrices are empty so here it is: ");
            return;
        }
        
        // declare an array for the matrix (next week in Programmieren)
        int[][] matrix = new int[n][n];
        
        // carefull: array are indexed from 0 to n-1 (Java) instead of 1 to n (LA)
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                matrix[i][j] = (int)(Math.random() * 10);
            }
        }
        
        System.out.println("Using randomly generated matrix A:");
        printMatrix(matrix);        

        // declare an array for the matrix, fill entries using sum formula
        int[][] squared = new int[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                squared[i][j] = 0;
                // here we want to compute c_ij using the formula
                for (int k = 0; k < n; k++) {
                    squared[i][j] += matrix[i][k] * matrix[k][j];
                }
            }
        }
        // phew this takes n^3 steps, which is really bad

        System.out.println("The square matrix A^2 is:");
        printMatrix(squared);        

        scan.close();
    }



}
