/**
* Ein Hello-World-Programm in Java.
* Dies ist ein Javadoc-Kommentar.
*
* @author John Doe
* @version 1.0
*/

public class DokumentationKommentare {
    /**
    * Hauptprogramm.
    *
    *@param args Kommandozeilenparameter
    */
    
    public static void main(String[] args) {
        
        int a = 1;  //a bekommt Wert 1 zugewiesen
        
        /* Veraenderungen an a haben im Kommentar keinen Effekt
        a = 2;  //So darf ich Kommentare schachteln
        */
        
        System.out.println (a); //unveraendert 1
        
        //Nun Veraenderung nicht als Kommentar:
        a = 2;
        
        System.out.println (a); //veraendert
    
    }
}
