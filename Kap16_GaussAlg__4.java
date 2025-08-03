import java.util.*;
public class Kap16_GaussAlg {
    
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
    
    //Methode um Vektor einzulesen
    static double [] vektorEinlesen (int n){
        
        // Erzeugt Vektor mit uebergebener Anzahl an Speicherstellen
        double [] einlesen = new double [n];
        Scanner scan = new Scanner (System.in);
        //Einlesen aller Komponenten
        for (int i = 0 ; i < n ; i++){
                einlesen [i] = scan.nextDouble ();
            }
        return einlesen;
    }
        
    
    
    public static void main (String [] args){
        
        Scanner scan = new Scanner (System.in); // Muss ueberall definiert werden
        
        //Dimension der Matrix/ des Vektors zum zug. LGS
        System.out.println ("Dimension?");
        int n = scan.nextInt ();
        
        // Einlesen der Matrizen mit Methode von oben
        System.out.println ("Geben Sie Ihre Matrix von links nach rechts und von oben nach unten ein!");
        double [][] a = matrixEinlesen (n,n);  
        
        //Einlesen des Vektors mit Methode von oben
        System.out.println ("Geben Sie Ihren Vektor ein!");
        double [] b = vektorEinlesen (n);  
        
        double f;
        // n-1 Eliminationsschritte
        for (int i = 0; i < n-1; i++) {
            // jeweils alle folgenden Zeilen bearbeiten
            for (int j = i+1; j < n; j++) {
                f = - a[j][i] / a[i][i];    // Faktor bestimmen
                a[j][i] = 0.0;      // dieses Element wird zu 0 gemacht
                
                    for (int k = i+1; k < n; k++){
                        // zum Rest der j-ten Zeile das f-fache der i-ten Zeile addieren
                        a[j][k] = a[j][k] + f * a[i][k];
                       }
                // ebenso auf der rechten Seite
                b[j] = b[j] + f * b[i];
                
                 }
            }
        
        double [] x = new double [n];
        //Löse a · x = b durch Einsetzen in richtiger Reihenfolge.
        for (int i = n-1; i >= 0; i--) {
            x[i] = b[i];    // rechte Seite
                for (int j = n-1; j > i; j--){
                     x[i] = x[i] - a[i][j] * x[j];   // alle bekannten x[j] einsetzen
                }
            x[i] = x[i] / a[i][i];      // durch Diagonalelement dividieren
            System.out.println (x[i]);  //Ausgabe der Lösung
        }
         
    }
}
