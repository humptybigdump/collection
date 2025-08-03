import java.util.*;

public class Kap9_Quersumme {

    public static void main(String [] args) {

        Scanner sc = new Scanner(System.in);

        int n, summe = 0;
        //Damit Nutzer weiss, was er eingeben soll
        System.out.print("n = ");
        
        //Einlesen der Variablen
        n = sc.nextInt();
        
        //Solange n positiv ist, addiere hinterste Ziffer, teile durch 10. 
        //Nutzt Integerdivision, damit keine Nachkommastellen/Rundungen
        while (n > 0)   {
        summe += n % 10;
        n /= 10;
        }

        System.out.println("Quersumme = " + summe);
    }
}