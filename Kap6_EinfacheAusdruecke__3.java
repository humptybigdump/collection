//Damit kann ich u.A. auf Mathfunktion zugreifen
import java.util.*;

public class Kap6_EinfacheAusdruecke {
    
    public static void main (String [] args){

	  // Einstellig:
       int a = +1; // + als Vorzeichen
       double b = -1; // - als Vorzeichen
       int c = a++; // s.6.3

        // Zweistellig:
       double d = a + b; // + steht f�r Addition, ergibt d = 0        
       double e = a - b; // - steht f�r Subtraktion, ergibt e = 2
       double f = a * b; // ergibt f = -1
        
        //Division mit Integer/Double (s.6.2)
        double a = 5.0/3.0; 
        System.out.println ("5.0/3.0 = " + a);
        double b = 5.0/3; 
        System.out.println ("5.0/3 = " + b);
        double c = 5/3.0; 
        System.out.println ("5/3.0 = " + c);
        double d = 5/3; 
        System.out.println ("5/3 = " + d); //Integerdiv.: Kann zu ungewollten Ergebnissen fuehren
        double e = 5/3.; 
        System.out.println ("5/3. = " + e);

        //Rest bei Division (s.6.2)
        double f = 5 % 3; 
        System.out.println ("5 % 3 = " + f);
        double g = (-5)/3;
        System.out.println ("(-5)/3 = " + g);
        double h = (-5) % 3; 
        System.out.println ("(-5) % 3 = " + h);
        double i = 3.5 % 1.1;
        System.out.println ("3.5 % 1.1 = " + i);
        
        //Inkrement- und Dekrementoperationen
        int j = 22;
        ++j;    //enstspricht j = j + 1;
        System.out.println ("Fuer j = 22 ergibt ++j: j = " + j);
        int k = 33;
        k++;    //enstspricht k = k + 1;
        System.out.println ("Fuer k = 33 ergibt k++: k = " + k);
        //Wichtig: Wirken aber u.U. verschieden!
        int l = 1; int m = ++l; // l wird zuerst erhoeht, dann wird der Wert m zugewiesen
        System.out.println ("l = " + l + ", m = " + m);
        l = 1; int n = l++; // l wieder auf 1, wird dann zuerst n zugewiesen, danach erst erhoeht
        System.out.println ("l = " + l + ", n = " + n);

        
       
         //Mathfunktion
        double o = Math.toRadians(180);
        System.out.println ("180 Grad enspricht im Bogenmaß " + o);
        
        //Typumwandlungen
        double p = (int) 1.9;
        System.out.println ("(int) 1.9 = " + p);
        double q = (int) -1.9; 
        System.out.println ("(int) -1.9 = " + q);
        double r = 5.35;
        System.out.println (r);
        double s = (int) r * 3;
        System.out.println ("(int) r * 3 = " + s);
        double t = (int) (r * 3);
        System.out.println ("(int) (r * 3) = " + t);
    }
}