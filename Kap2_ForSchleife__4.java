// die Klasse des Scanners wird bekannt gemacht
import java.util.*; 

    public class ForSchleife{
        
        public static void main (String [] args) {
            
        //Eingabe wird auf US-Stil umgestellt; Trennung von ganzer Zahl und Nachkommastellen durch einen Punkt 
        Locale.setDefault (Locale.US); 
        
        // Scanner wird erzeugt, bekommt Namen sc
        Scanner sc = new Scanner (System.in); 
        
       //for-Schleife:
        /*Faengt mit Wert i = 0 an
        Gibt den Wert i aus und eröht ihn danach um 1
        Dieser Vorgang wiederholt sich solange, wie i < 12 ist*/
        for (int i = 0 ; i < 12 ; i++){
            System.out.println (i);
        }
    }
}