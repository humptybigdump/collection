// Dieser Code hält sich nicht an die Bewertungsrichtlinien!

public class TreePattern {
    public static void main(String[] args) {
        int rows = 5; // Anzahl der Zeilen im Muster

        for (int i = 1; i <= rows; i++) { // Äußere Schleife für die Zeilen
            // Schleife für dashes vor den Sternen
            for (int j = rows - i; j > 0; j--) {
                System.out.print("-");
            }

            // Schleife für Sterne
            for (int k = 1; k <= (2 * i - 1); k++) {
                System.out.print("*");
            }

            // Schleife für dashes nach den Sternen
            for (int j = rows - i; j > 0; j--) {
                System.out.print("-");
            }

            // Zeilenumbruch nach jeder Zeile
            System.out.println();
        }
    }
}
