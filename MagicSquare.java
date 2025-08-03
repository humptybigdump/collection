// Dieser Code hält sich nicht an die Bewertungsrichtlinien!

import java.util.Scanner;

public class MagicSquare {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        // Eingabe: Größe der Matrix
        System.out.print("Geben Sie die Größe der Matrix ein (n x n): ");
        int n = scanner.nextInt();

        // Eingabe: Matrix
        int[][] matrix = new int[n][n];
        System.out.println("Geben Sie die Matrix ein:");
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                matrix[i][j] = scanner.nextInt();
            }
        }

        // Summenberechnung in einer einzigen Schleife
        int mainDiagonalSum = 0;
        int secondaryDiagonalSum = 0;
        int magicSum = -1;
        boolean isMagicSquare = true;

        for (int i = 0; i < n; i++) {
            int rowSum = 0;   // Summe der aktuellen Zeile
            int colSum = 0;   // Summe der aktuellen Spalte

            for (int j = 0; j < n; j++) {
                rowSum += matrix[i][j];   // Zeilensumme
                colSum += matrix[j][i];   // Spaltensumme
            }

            // Diagonalen berechnen
            mainDiagonalSum += matrix[i][i];
            secondaryDiagonalSum += matrix[i][n - i - 1];

            // Überprüfen der Zeilen- und Spaltensummen
            if (magicSum == -1) {
                magicSum = rowSum;  // Setze die magische Summe
            }
            if (rowSum != magicSum || colSum != magicSum) {
                isMagicSquare = false;
                break;
            }
        }

        // Überprüfen der Diagonalsummen
        if (mainDiagonalSum != magicSum || secondaryDiagonalSum != magicSum) {
            isMagicSquare = false;
        }

        // Ergebnis
        if (isMagicSquare) {
            System.out.println("Die Matrix ist ein magisches Quadrat!");
        } else {
            System.out.println("Die Matrix ist KEIN magisches Quadrat.");
        }
    }
}
