// Dieser Code hält sich nicht an die Bewertungsrichtlinien!

public class PrimeNumbers {
    public static void main(String[] args) {
        System.out.println("Primzahlen zwischen 1 und 100:");

        // Schleife über alle Zahlen von 2 bis 100
        for (int number = 2; number <= 100; number++) {
            boolean isPrime = true;

            // Prüfen, ob die Zahl durch eine kleinere Zahl (außer 1) teilbar ist
            for (int divisor = 2; divisor <= Math.sqrt(number); divisor++) {
                if (number % divisor == 0) {
                    isPrime = false; // Wenn teilbar, ist die Zahl keine Primzahl
                    break; // Schleife abbrechen, weitere Prüfungen unnötig
                }
            }

            // Ausgabe der Zahl, wenn sie eine Primzahl ist
            if (isPrime) {
                System.out.print(number + " ");
            }
        }
    }
}
