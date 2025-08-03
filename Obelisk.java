import java.util.*; // wird wegen "Scanner" und "Math" benoetigt

/* Berechnung von
 *   Mantelflaeche M
 *   Oberflaeche O
 *   Volumen V
 * eines Obelisken
 * Eingabe: a,b,c,d (Seitenlaengen) h (Hoehe)
 * Ausgabe: M, O, V
 * Author: Stefan Findeisen 2016
 */

public class Obelisk {
    public static void main(String[] args)
    {
        /* Setze die Standardeinstellungen */
        Locale.setDefault(Locale.US);

        /* Daten des Obelisken */
        double a, b, c, d; // Seitenlaengen
        double h; // Hoehe

        /* Eingabe */
        /* Hilfstext: */
        System.out.println("Geben Sie die Seitenlaengen a, b, c und d " +
                           "sowie die Hoehe h eines Obelisken ein.");

        /* Eigentliche Eingabe mit Hilfstexten */
        Scanner sc = new Scanner(System.in);

        System.out.print("a = ");
        a = sc.nextDouble();
        System.out.print("b = ");
        b = sc.nextDouble();
        System.out.print("c = ");
        c = sc.nextDouble();
        System.out.print("d = ");
        d = sc.nextDouble();
        System.out.print("h = ");
        h = sc.nextDouble();

        /* Berechung */
        double G1 = a * b;
        double G2 = c * d;
        double s1 = 0.5 * (a + c);
        double s2 = 0.5 * (b + d);
        double t1 = 0.5 * (b - d);
        double t2 = 0.5 * (a - c);
        double A1 = s1 * Math.sqrt(h * h + t1 * t1);
        double A2 = s2 * Math.sqrt(h * h + t2 * t2);

        double M = 2 * A1 + 2 * A2;
        double O = M + G1 + G2;
        double V = h / 6 * (G1 + (a + c) * (b + d) + G2);

        /* Ausgabe */
        System.out.println("Mantelflaeche = " + M);
        System.out.println("Oberflaeche = " + O);
        System.out.println("Volumen = " + V);
    }
}
