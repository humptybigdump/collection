//Erweiterung zum Beispiel GgT

//Damit Scanner u. ae. Funktionen genutzt werden k�nnen
import java.util.*;

public class GgT{

    public static void main (String [] args){ 
        
        //Importiere Scanner
        //"scan" ist dabei nur ein bel. Name um den Scanner aufzurufen
        Scanner scan = new Scanner (System.in);
    
        //Fordere Nutzer auf, Zahlen einzugeben
        System.out.println ("Geben Sie 2 ganze Zahlen ein die ungleich 0 sind");
        
        //Lese diese Variablen von der Konsole ein
        int a = scan.nextInt();
        int b = scan.nextInt();
        
        //Fuer die spaetere Ausgabe, da a und b veraendert werden
        int m = a;
        int n = b;
        
        //Deklariere Variable h, keine Initialisierung
        int h;

        //Wenn b != (ungleich) 0
        while (b != 0){
           
            //Schritte 2-4
            h = a % b;
            a = b;
            b = h;
            
            }
        
        //Gebe a als GgT auf der Konsole aus
        System.out.println ("Ein GgT von " + m + " und " + n + " ist " + a);    //Stringkonketation
        
        /*GgT im reellen nur bis auf Vorzeichen eindeutig
        Um positiven GgT zu erhalten:
        Multipliziere -1 falls a < 0 */
        if (a < 0){
            a = (-1) * a;
        }
        
        System.out.println ("Der positive GgT ist " + a);
        
    }
}
