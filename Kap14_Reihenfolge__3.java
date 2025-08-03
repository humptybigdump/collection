public class Kap14_Reihenfolge {
    
    public static void main(String[] args) {
        
        // Beachte Reihenfolge der Operatoren/ Zuweisungen
        // Erzeugen vom Feld
        int[] feld = {1,2,3,4};
        int i = 0;
        System.out.println(feld[++i] + feld[++i] * feld[++i]);  // feld[ 1 ] + feld[ 2 ] * feld[ 3 ]
        
        //Index wieder auf 0 setzen, feld wie urspruenglich
        i = 0;
        feld[0] = 1;feld[1] =2; feld [2] = 3;feld [3] = 4;
        //Unterschied zwischen Inkrement und Dekrement (Kap. 6.4)
        System.out.println(feld[i++] + feld[i++] * feld[i++]);  //feld [0]+ feld [1] *feld[2];
       
        // Index wieder auf 0 setzen
        i = 0;
        feld [++i] = feld[++i] = feld[++i]; // feld [ 1 ] = feld[ 2 ] = feld[ 3 ]; Zuweisung von rechts nach links
        System.out.println (feld [0] + " " + feld[1] + " " + feld[2] + " " + feld[3]);

     }
}