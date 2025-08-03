// die Klasse des Scanners wird bekannt gemacht
import java.util.*; 

    public class IfAbfrage{
        
        public static void main (String [] args) {
            
        //Eingabe wird auf US-Stil umgestellt; Trennung von ganzer Zahl und Nachkommastellen durch einen Punkt 
        Locale.setDefault (Locale.US); 
        
        // Scanner wird erzeugt, bekommt Namen sc
        Scanner sc = new Scanner (System.in); 
        
        //Aufforderung zur Eingabe einer ganzen Zahl
        System.out.println ("Geben Sie eine ganze Zahl ein!");
        int i = sc.nextInt();
        
        //Bedingte Anweisung
        if (i < 0){ //falls i klein genug
            System.out.println ("Ihre Zahl ist negativ");
        }
            else{ //falls i zu gross
                System.out.println ("Ihre Zahl ist positiv");
            }
        /*Beachte: Hier zaehlt 0 zu positiv. 
        Moechte man 0 seperat haben kann man mehrere Bedingungen setzen:*/
        
        if (i < 0){ //falls i klein genug
            System.out.println ("Ihre Zahl ist negativ");
            }
            else if (i > 0){ //falls i zu gross
                System.out.println ("Ihre Zahl ist positiv");
                }
                else{
                    System.out.println ("Ihre Zahl ist 0");
                }
        
    }
}