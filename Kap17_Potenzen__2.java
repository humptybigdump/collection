public class Kap17_Potenzen{
 
    //Kap. 17.2
    // Methode1
    static double power (double x, int n) {
    if (n > 0)
        return power (x, n-1) * x; // Rekursion, ruft die Methode wieder auf
    else
    return 1; // nicht rekursiver Zweig, damit finit
    }
    
    
    // Methode2
    static double power1 (double x, int n) {
    if (n <= 0)
        return 1;   // nicht rekursiver Zweig, damit finit
    if (n%2 != 0)   // ungerade
        return power1 (x, n-1) * x;     // Rekursion, ruft Methode erneut auf
    else {double d = power1 (x, n/2);   // Rekursion, beschleunigt Alg. im Ggsatz zu oben
        return d*d;
        }
    }
    
    public static void main (String [] args){
        
        double x = 5;
        int n = 6;
        
        //Aufrufen beider Methoden
        double p = power (x , n);
        double p1 = power1 (x , n);
        
        //Ergibt das gleiche Ergebnis
        System.out.println(p);
        System.out.println(p1);
        
        /*Bei kleinen Werten fuer n merkt man keinen Unterschied zwischen den Alg.
        Bei grossen Werten ist der 2. Alg. allerdings deutlich schneller, da bei
        geraden Zahlen n halbiert wird: Logarithmischer Aufwand
        */
    }
}