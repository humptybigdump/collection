public class Kap12_Potenz {
    
    static double power(double x, int n) {
        
        // Absolut-Betrag von n
        int m = Math.abs(n); 
        double y = 1.0;
        
        // Berechnung von x^m
        // x m mal mit sich selbst multiplizieren
        for (int i = 0; i < m; ++i){
            y *= x;
        }
            
        //Wichtig: Fuer jede Moeglichkeit ein return!
        if (n >= 0){// Original-n
             return y;
        } 
        else{
            return 1.0/y; // Ergebnis eventuell invertieren
        }
            
            }
    
    
        public static void main(String[] args) {
        
            //Zugriff auf Methode
            double a = power (-2 , -2);
            System.out.println (a);
    }
}