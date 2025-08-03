public class Kap5_Variablen {
    public static void main (String [] args) {
        
        //Deklaration
        int a, b, c;
        double d, e;
        boolean f;
        
        //nachtraegliche Initialisierung
        a = 1;  
        d = 0.028;
        f = false;
        
        //Deklaration und Initialisierung 
        double wert = 123.45;   // okay
        int dim = 10;   // okay
        int i, j = 10, k;   // Nur j wird initialisiert, i, k bekommen keinen Wert zugewiesen
        double quad = wert*wert;     // okay
        int dim1 = dim + 1;    // okay
        double doubleDim = dim;     // okay, dim ist int
        /*
        int sum = i+j; // Fehler: i nicht initialisiert
        int intWert = wert; // Fehler: wert ist double
        */
        int intWert2 = (int)wert;   // okay, Typkonversion per Typecast (siehe: Ausdruecke, Kap.6)
        wert = 345.67;  // okay, wert ist keine Konstante, darf veraendert werden
        
        
        //Symbolische Konstanten
        final int n = 5;
        final char newline = '\n';
        final double x;     // nur Deklaration, noch keine Initialisierung
        x = 123.456;    // nachtraegliche Zuweisung
        /*
        n = 6;    // Fehler: n darf nicht veraendert werden
        x = 3.14159;    // Fehler: x darf nicht mehr veraendert werden
        */

    }
}