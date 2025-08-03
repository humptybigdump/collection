public class Kap12_Summe {
    
    //Berechnet Summe von bel. vielen Parametern
    static double sum(double... x) {
            double s = 0.0;
            for (int i = 0; i < x.length; i++){
                  s += x[i];
                }            
            return s;
            }
    
        public static void main(String[] args) {
        
            // Legale Aufrufe:
            double a = sum(); // Liefert 0 als Wert, kein Fehler
            System.out.println (a);
            double b = sum(1); // liefert nur Zahl selbst
            System.out.println (b);
            double c = sum(2,3); // addiert 2 Zahlen
            System.out.println (c);
            double d = sum(1,2,3); // addiert 3 Zahlen
            System.out.println (d);

            double [] vek = {2, 3, 5};
            double e = sum(vek); // addiert alle Feldkomponenten
            System.out.println (e);
    }
}