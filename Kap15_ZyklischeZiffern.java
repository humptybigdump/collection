import java.util.*;
public class Kap15_ZyklischeZiffern {
    
    public static void main (String [] args) {

        Locale.setDefault(Locale.US);
        Scanner sc = new Scanner(System.in);
        
        //Defninitionen
        int faktor, ziffer, uebertrag=0, produkt, stellen=0;
        System.out.print("Mit welchem Faktor soll multipliziert werden?");
        faktor = sc.nextInt();      // Faktor einlesen
        ziffer = faktor;    // niederwertigste Ziffer = Faktor
        System.out.println("Ergebnis lautet (rueckwaerts gelesen !):");

        do{
            System.out.print(ziffer);   // aktuelle Ziffer ausdrucken 
            stellen ++;     // Stellenzahl bei jeder Ausgabe erhoehen
            produkt = ziffer * faktor + uebertrag;      // neues Produkt, zweistellig
            ziffer = produkt % 10;  // neue Ziffer = niederste Stelle des Produkts
            uebertrag = produkt / 10;   // neuer Uebertrag = hoechste Stelle des Produkts
        }
        // wiederholen, solange eine der beiden Abbruch-Bedingungen verletzt ist
        while 
        (ziffer != faktor || uebertrag != 0);
        
        System.out.println();   //Leerzeile
        System.out.println("Es hat " + stellen + " Stellen");   //Stellenanzahl;
    }
}

