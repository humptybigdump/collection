//Zu 7.4
//Damit Scaanner u.Ae. verwendet werden kann
import java.util.*;

    public class EinfacheEingabeanweisung {
        public static void main(String [] args) {
            
            //US-Dezimaltrennung (Punkt zwischen Zahl und Nachkommastellen)
             Locale.setDefault(Locale.US);
             
             //Scanner bekanntmachen, Bezeichnung als sc
            Scanner sc = new Scanner(System.in);
            
            //Zur Eingabe auffordern
            System.out.print("Bitte i eingeben: ");
            //Eingabe ist Variable i
            int i = sc.nextInt();
            //Ausgabe
            System.out.println("i*2=" + i*2);
        }
}
