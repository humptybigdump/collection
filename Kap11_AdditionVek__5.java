import java.util.*;
public class AddVek {

    public static void main(String[] args) {
        
        Locale.setDefault(Locale.US);
        Scanner sc = new Scanner(System.in);
        
        //Felder anlegen
        double[] a, b;
        //Dimension der Felder
        System.out.println("Dimension = ");
        int dim = sc.nextInt();
        
        //Felder mit gewählter Dimension anlegen
        a = new double[dim];
        b = new double[dim];
        
        /*Einlesen der Vektoren
        Fuer jede Komponente eigene Aufforderung
        */
        for (int i = 0; i < a.length; ++i) { //ueber a.length bestimmt, wieviele Komponenten es sind
            System.out.print("a ["+i+ "]: ");
            a[i] = sc.nextDouble();
            System.out.print("b ["+i+ "]: ");
            b[i] = sc.nextDouble();
            }
        
        //Vektoraddition
        for (int i = 0; i < a.length; ++i){
            System.out.println("Summe["+i+"]: "+(a[i]+b[i]));
        }
            
    }
}