public class forSchleife {
    
    public static void main(String args[]) {
    
       //Jede Stelle gefuellt; ist oft am Uebersichtlichsten
       for (int h = 0; h < 10 ; h++){
           System.out.print (h + " ");
        }
        System.out.println ();  //Zeilenumbruch
        
        //1.Stelle ausserhalb
        int i = 5;
        for (; i < 10; ++i) {
        System.out.print(i + " ");  //  Var. i gilt ueber Schleifen-Ende hinaus.
        }
        System.out.println ();  //Zeilenumbruch
        
        //2.Stelle ausserhalb
        for (int j = 0;; ++j){  //Variable j gilt nur bis Schleifen-Ende
           if (j < 10) {
               System.out.print(j + " ");
           }
            else break;  // if-else-Anweisung ersetzt die Abbruchbedingung.
        }
        System.out.println ();  //Zeilenumbruch
        
        //3.Stelle ausserhalb
        /*j kann erneut definiert werden, da es oben nur bis Schleifen-Ende gilt.
        Es duerfte z.B. nicht erneut i definiert werden*/
        for (int j = 0; j < 10; ) {
            System.out.print(++j + " ");    // das Update ist in der Ausgabe enthalten
                                        // durch Inkrement andere Ausgabe als in Schleife davor
        }
        System.out.println ();  //Zeilenumbruch
    
        //Alles ausserhalb. Funktioniert, ist aber oft sehr unuebersichtlich
        int k = 5;
        for (;;){
             if (k < 10) {
                 System.out.print(k++ + " ");
             }
        else break;
        }
    
    }
}