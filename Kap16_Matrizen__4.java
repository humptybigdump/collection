import java.util.*;
public class Kap16_Matrizen {
    
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
        System.out.println ("Geben Sie Ihre Matrizen von links nach rechts und von oben nach unten ein!");
        double [][] A = matrixEinlesen (3,3);   //Zahlen in Klammer geben Anzahl an Zeilen, Spalten
        double [][] B = matrixEinlesen (3,3);    //koennen hier bel. int-Werte sein
        
        //Summe:
        //geht davon aus, dass Matrizen gleiche Groesse haben
        double[][] S = new double [A.length][A[0].length];  
        
        for (int i = 0; i < A.length; i++){     // soviele Zeilen wie x
            for (int j = 0; j < A[0].length; j++){      // soviele Spalten wie x in jew. Zeile
                S[i][j] = A[i][j] + B[i][j];        //Summiere Eintraege
                System.out.print (S[i][j] + "   ");  //Eintraege innerhalb einer Zeile mit etwas Abstand zwischen ihnen
            }
            System.out.println ();  //Nach Zeilenende folgt ein Zeilenumbruch
        }
        
        //Skalarmultiplikation
        //Auch hier muss ueber jeden Eintrag gegangen werden,
        //also ueber alle Spalten jeder Zeile
        double lambda = 4;
        double [][] Sm = new double [A.length][A[0].length];  //gleiche Groesse wie A
        
        for (int i = 0; i < A.length; i++){
            for (int j = 0; j < A[0].length; j++){
                Sm [i][j] = lambda * A[i][j];
                System.out.print (Sm [i][j] + "   ");    //Ausgabe der Werte innerhalb einer Zeile
            }
            System.out.println ();  //Zeilenumbruch
        }
            
                
        //Matrixprodukt: Wichtig fuer viele weitere Aufgaben!
        /* Zeilenzahl von c = Zeilenzahl (ZZ) von a
         Spaltenzahl von c = Spaltenzahl (SZ) von b
         wichtig fuer Matrixenmultiplikation! */
        double[][] c = new double [A.length][B[0].length];
        
        // Ergebnis in allen Zeilen und Spalten
        for (int i = 0; i < A.length; i++){
            for (int j = 0; j < B[i].length; j++) {
                // Skalarprodukt des i-ten Zeilenvektors von A und des j-ten Spaltenvektors von B berechnen
                double s = 0.0;
                for (int k = 0; k < B.length; k++){
                    s += A[i][k] * B[k][j];
                    }    
                    c[i][j] = s;
                System.out.print (c[i][j] + "   ");
            }
            System.out.println ();
        }
            
    }
}

