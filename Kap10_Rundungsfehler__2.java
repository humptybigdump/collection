public class Kap10_Rundungsfehler {

    public static void main(String [] args) {
        
        //Definiere Variablen vom Typ float zur Berechnung
        float p = 665857;
        float q = 470832;
        
        //Berechne Ergebnis mit float
        float f = p * p - 2 * q * q;
        System.out.println (f);
        
        //Definiere Variablen vom Typ double zur Berechnung
        double a = 665857.0;
        double b = 470832.0;
        
        //Berechne Ergebnis mit double
        double d = a * a - 2 * b * b;
        System.out.println (d);
        
        /*Liefert verschiedene Ergebnisse!
        Liegt daran, dass bei Variablen vom Typ double mehr Speicherplatz reserviert wird
        Damit ist eine genaue Berechnung moeglich.
        Problem: Quadriert man die Gleichung kommt es auch fuer double zu zu grossen
        Zwischenergebnissen, damit zu einem Ueberlauf und einem falschen Ergebnis (s. u.)
        */
        
        double d2 =  (a * a * a * a) - 4 * (a * a * b * b) + 4 * (b * b * b * b);
        System.out.println (d2);
    }
}