// die Klasse des Scanners wird bekannt gemacht
import java.util.*; 

    public class Kap2_DoWhileSchleife{
        
        public static void main (String [] args) {
            
        //Eingabe wird auf US-Stil umgestellt; Trennung von ganzer Zahl und Nachkommastellen durch einen Punkt 
        Locale.setDefault (Locale.US); 
        
        //bel. Variable festlegen
        int i = 1;  //Tipp: Pruefe auch mit i = 7 um Unterschied zur while-Schleife zu sehen
        
       //do-while-Schleife
       /*Die Schleife wird fuer jedes i mindestens 1 mal durchlaufen, egal ob Aussage stimmt oder nicht! 
       !(Unterschied zur while-Schleife)!
       Dann wird i um 1 erhöht
       Erst danach folgt die Pruefung der Bedingung, ob noch ein weiterer Schleifendurchlauf stattfindet*/
       
       do {
           System.out.println (i + " ist kleiner als 5");
           i = i + 1;
       }
       while (i < 5);
        
    }
}