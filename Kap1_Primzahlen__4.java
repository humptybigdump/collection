// alle Primzahlen von 2 bis 999 ausgeben
public class Kap1_Primzahlen {
    public static void main (String [] args) {
        
        // hier steht der Algorithmus:

        // Variablendeklaration
        int zahl; // Primzahl-Kandidat
        int teiler; // moegliche Teiler
        boolean prim; // Gibt an, ob eine Zahl prim ist, oder nicht
        
        // erst mal vorab: 2 ist prim
        System.out.print (2 + " "); // 2 und Leerzeichen ausgeben
        
        // dann alle ungeraden Zahlen von 3 bis 999 testen
        //nur ungerade Primkandidaten, da sonst durch 2 teilbar
        zahl = 3; // Startwert 3
        // Schleife, die alle ungeraden Zahlen bis 999 testet
        while (zahl <= 999) { // solange zahl noch im gewuenschten Bereich ist
            prim = true; // anfangs noch kein Teiler gefunden
            teiler = 3; 
            /* es kommen nur Teiler <= Wurzel(zahl) in Frage,
             wenn Teiler gefunden wurde (prim = false ist),
            Test abbrechen */
            while (prim == true && teiler * teiler <= zahl) { // weitertesten?
                if (zahl % teiler == 0){ // teilbar?
                    prim = false; // dann ist es keine Primzahl
                    teiler = teiler + 2; // naechsten ungerade Teiler ausprobieren
                                        //nur ungerade Teiler, da ungerade Zahl nie durch gerade Zahl teilbar
                  }  
              }    
                if (prim == true) {// wenn keine Teiler gefunden wurden
                    System.out.print (zahl + " "); // zahl ausgeben und Leerzeichen
                    
                    }
           
            zahl = zahl + 2; // naechste ungerade Zahl
        }
    }
}