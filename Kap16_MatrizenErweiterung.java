import java.util.*;
public class Kap16_MatrizenErweiterung {
    
    //Methode um eine Matrix einzulesen.
    //bekommt 2 natuerliche Zahlen als Parameter
    static double [][] matrixEinlesen (int m, int n){
        
        // Erzeugt Matrix mit vorgegebener Groesse
        double [][] einlesen = new double [m][n];
        Scanner scan = new Scanner (System.in);
        for (int i = 0 ; i < m ; i++){
            for (int j = 0 ; j < n ; j++){
                einlesen [i][j] = scan.nextDouble ();
            }
        }
        return einlesen;
    }
        
    
    
    public static void main (String [] args){
        
        // Einlesen der Matrizen mit Methode von oben
        System.out.println ("Geben Sie Ihre Matrix ein!");
        double [][] x = matrixEinlesen (3,3);   //Zahlen in Klammer geben Anzahl an Zeilen, Spalten
                                       //koennen hier bel. int-Werte sein
        
        // Transponierte t zu x anlegen
        double[][] t = new double[x[0].length][x.length];   // ZZ ist die SZ von x und die SZ ist die ZZ von y
        System.out.println ("Die Transponierte zu x sieht folgendermassen aus:");
        //Gehe wieder ueber alle Zeilen und Spalten
        for (int i = 0; i < x[0].length; i++){
            for (int j = 0; j < x.length; j++){
                t[i][j] = x[j][i];  //i,j-ter Eintrag von x wird zu j,i-tem Eintag von t
                System.out.print (t [i][j] + "   ");
            }
            System.out.println ();
        }
        
        
        // p-Norm
        //p ganzzahlig, 1 < p < unendlich
        int p = 5;
        double pnorm;   //Norm ist auch hier wieder ein Skalar!
        double sum = 0.0;
    
        // Gehe ueber alle Zeilen mit jew. allen Spalten
        for (int i = 0; i < x.length; i++){
            for (int j = 0; j < x[i].length; j++){
                //Berechne p-te Potenz von jew. Matrixeintrag, addiere diese auf
                sum += Math.pow (Math.abs(x[i][j]), p);
            }
           } 
        //Ziehe n-te Wurzel
        pnorm = Math.pow (sum, 1.0/p);   // NICHT 1/p; Integerdivision!
        //Ausgabe:
        System.out.println ("p- Norm ist: " + pnorm);
     
     
        //Zeilensummennorm
        double norm = 0.0;
        //Gehe ueber jeden Zeile
        for (int i = 0; i < x.length; i++) {
            double sum0 = 0.0;   //Starte immer mit Wert 0
            //Addiere alle Eintraege der i-ten Zeile betragsmaessig auf
            for (int j = 0; j < x[0].length; j++){
                 sum0 += Math.abs(x[i][j]); 
            }
            //Wenn groesser ist, als die davor, ueberschreibe diesen Wert
            //Damit wird Maximum aller Summen gefunden; das entspricht nach Def. der Norm
            if (sum0 > norm){
                norm = sum0;
                }
            }
        System.out.println("Die Zeilensummennorm von x betraegt: " + norm);
        
        
        // Spaltensummennorm
        // funktioniert genau gleich, nur dass man erst ueber jede Spalte geht
        // und alle Eintraege der Spalte addiert.
        double norm1 = 0.0;
        //alle Spalten
        for (int j = 0; j < x[0].length; j++) {
            double sum1 = 0.0;
            //alle Eintraege der jew. Spalte aufsummieren
            for (int i = 0; i < x.length; i++){
                sum1 += Math.abs(x[i][j]);
                }
            //groessten Wert als Norm festlegen
            if (sum1 > norm1){
                norm1 = sum1;
                }
            }
        System.out.println ("Die Spaltensummennorm von x betraegt: " + norm1);
        
        /*Bemerkung:
        Zeilensummennorm der Transponierten entspricht Spaltensummennorm 
        der urspruenglichen Matrix und anderstrum
        */
         
    }
}
