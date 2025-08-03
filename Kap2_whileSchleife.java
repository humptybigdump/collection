// die Klasse des Scanners wird bekannt gemacht
import java.util.*; 

    public class WhileSchleife{
        
        public static void main (String [] args) {
            
        //Eingabe wird auf US-Stil umgestellt; Trennung von ganzer Zahl und Nachkommastellen durch einen Punkt 
        Locale.setDefault (Locale.US); 
        
        //bel. Variable festlegen
        int i = 1;
        
       //while-Schleife
       /*Solange i kleiner ist als 5 wird der Satz ausgegeben
       Bei jedem Durchlauf wird i um 1 erhoeht und die Pruefung folgt erneut
       Das Programm endet, sobald i = 5 ist. Achte auf Finitheit!*/
       
       while (i < 5){
           System.out.println (i + " ist kleiner als 5");
           i = i + 1;
       }
        
    }
}