// Dieser Code hält sich nicht an die Bewertungsrichtlinien!

public class SumLoop {
    public static void main(String[] args) {
        int upperBound = Integer.parseInt(args[0]);

        // Summieren mit einer for-Schleife
        int sumFor = 0;
        for (int i = 1; i <= upperBound; i++) {
            sumFor += i;
        }
        System.out.println("Summe mit for-Schleife: " + sumFor);

        // Summieren mit einer while-Schleife
        int sumWhile = 0;
        int i = 1;
        while (i <= upperBound) {
            sumWhile += i;
            i++;
        }
        System.out.println("Summe mit while-Schleife: " + sumWhile);
    }
}
