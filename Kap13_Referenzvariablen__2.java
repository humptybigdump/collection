public class Kap13_Referenzvariablen {
    
    static void demo1(int[] x) {
        x[0]++;
        System.out.println (x[0]);
        }



   static void demo2(int[] x) {
        x = new int[] {4, 5, 6}; // neues Objekt, nur in Methode definiert
        System.out.println(x[0]); // Ausgabe: 4, bezieht sich auf Feld in Methode definiert
    }
    
    public static void main(String[] args) {
       // Aufruf Methode 1
        int[] y1 = {1, 2}; // d.h. y1[0] = 1
        demo1(y1); // Aufruf der Methode demo1, gibt Wert der Variable in der Methode aus
        // jetzt: y1[0] = 2, d.h. referenzierter Wert wird veraendert
        System.out.println (y1[0]);

      // Aufruf Methode 2
        int[] y = {1,2}; // d.h y[0] = 1
        demo2(y); // Aufruf der Methode demo2, gibt Wert von Variable in der Methode aus
        
        //Lebensdauer von Feld in Methode endet dann wieder, kein return!
        // y[0] = 1 unveraendert, Referenzvariable kann nicht veraendert werden
        System.out.println(y[0]);

     }
}