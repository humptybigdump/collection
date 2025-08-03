// die Klasse des Scanners wird bekannt gemacht
import java.util.*; 
	
	//Der Programmname ist hier 'Grundalgorithmen'
    public class Kap2_Grundalgorithmen {
        
        public static void main (String [] args) {
            
        //Eingabe wird auf US-Stil umgestellt; Trennung von ganzer Zahl und Nachkommastellen durch einen Punkt 
        Locale.setDefault (Locale.US); 
        
        // Scanner wird erzeugt, bekommt Namen sc
        Scanner sc = new Scanner (System.in); 
        
        //Aufforderung zur Eingabe, beachte: US-Stil
        System.out.println ("Geben Sie zuerst eine ganze Zahl, dann eine Gleitkommazahl ein!");   //Ausgabeanweisung
        
        //Hier wird Speicherplatz fuer die Variablen reserviert
        int a;
        double b;
        double c;
        
        //Hier erfolgt eine Wertzuweisung fuer die Variablen
        a = sc.nextInt();   //Eingabeanweisung
        b = sc.nextDouble();    //Eingabeanweisung
        
        // hier steht der Algorithmus (Folge von Anweisungen) des Programms
        c = a * b;  //Wertzuweisung
        
        //Ergebnis ausgeben
        System.out.println ("Das Produkt der beiden Zahlen ist " + c);  //Ausgabeanweisung
        
    }
}