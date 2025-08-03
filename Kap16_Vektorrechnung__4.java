import java.util.*;
public class Kap16_Vektorrechnung {
    
    public static void main (String [] args) {
        
        // Lege 2 Vektoren gleicher Laenge an
        int[] x = {1, 2, 3};
        int[] y = {3, 2, 3};
        
        // Vektoraddition:
        // Neuer Vektor, soll gleich viele Speicherstellen haben wie x
        double[] s = new double[x.length];
        // Fuelle Speicherstellen von s mit der Summe der jew. Eintraege von x und y
        for (int i = 0; i < x.length; i++){
            s[i] = x[i] + y[i];
            //Gebe Komponenten aus
            System.out.println (s[i]);
        }
        
        System.out.println ();  //Leerzeile (Uebersichtlichkeit)
        //Skalarmultiplikation:
        double lambda = 5.5;
        // Neuer Vektor, soll gleich viele Speicherstellen haben wie x
        double[] m = new double [x.length];
        //Fuelle Speicherstellen von m mit dem Produkt der Komponenten mit lambda
        for (int i = 0; i < x.length; i++){
            m[i] = lambda * x[i];
            //Gib Komponenten aus
            System.out.println (m[i]);
        }
        
        System.out.println();
        // Skalarprodukt:
        double p = 0.0; //Ergibt keinen Vektor, sondern Skalar. 
                    //Setze am Anfang 0 damit definiert, aendert nichts am Wert der Summe
        // Summiere komponentenweise miteinander multiplizierte Eintraege von x und y
        for (int i = 0; i < x.length; i++){
            p += x[i] * y[i];   
        }
        //Einmalige Ausgabe am Ende reicht, ergibt ja nur einen Wert 
        System.out.println (p);
            
    }
}

