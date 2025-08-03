import java.util.*;
public class Kap14AdditionMat {

    public static void main(String[] args) {
        
        Locale.setDefault(Locale.US);
        Scanner sc = new Scanner(System.in);
        
        //Anlegen der Felder
        double[][] a, b;
        
        //Dimension einlesen und den Feldern zuschreiben
        System.out.print("Dimension = ");
        int dim = sc.nextInt();
        a = new double[dim][dim];   //ergeben quadratische Matrizen
        b = new double[dim][dim];
        
        //Einlesen der Werte
        System.out.println ("Geben Sie die i,j-ten Werte von a und b jeweils nacheinander ein");
        for (int i = 0; i < a.length; ++i) {    //alle Zeilen
            for (int j = 0; j < a[i].length; ++j) { //alle Spalten
                a[i][j] = sc.nextDouble();
                b[i][j] = sc.nextDouble();
                }
            }
            
        //Ausgabe der Summe der Matrizen, durch Zeilenumbrueche wie gewaehlt auch in Matrixform
        for (int i = 0; i < a.length; ++i) {    //Alle Zeilen
            for (int j = 0; j < a[i].length; ++j){  //Alle Spalten
                System.out.print((a[i][j]+b[i][j]) + "    ");   //jeweiligen Eintrag mit einer Leerstelle dazwischen ausgeben
                 }
            System.out.println();   //Wenn alle Spalten einer Zeile dargestellt sind, erfolgt ein Zeilenumbruch
              }
 
    }
}