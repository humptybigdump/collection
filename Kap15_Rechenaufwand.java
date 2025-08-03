public class Kap15_Rechenaufwand{
    
    public static void main(String[] args) {
        
        //Zuweisung, zaehlt nicht
        double ergebnis = 0;
        int n = 5;  //n muss hier definiert werden, damit Algorithmus definiert ist
                // Beim Abschaetzen des Rechenaufwands betrachtet man die asymptotische 
                // Annaeherung an eine Funktion fuer grosse n
        
        //Schleife, die n mal durchlaufen wird
        for(int i=0;i<n;i++) {
            ergebnis += 2.0 * i;    //jeweils 2 Operationen (+ und *)
            }
        ergebnis = ergebnis % n;    //% und = zaehlen nicht als Operationen
        
        
        //Neuzuweisung, zaehlt nicht
        ergebnis = 2;
        //Schleife, die 2n mal durchlaufen wird
        for(int i=1;i<=2*n;i++) {
            ergebnis *= ergebnis;   //1 Operation (*)
            //geschachtelte Schleife, wird i mal durchlaufen
            for(int j=0;j<i;j++) {
                ergebnis += 1;  //1 Operation (+)
                }
            }
        //Vergleich zaehlt nicht als Operation
        if(ergebnis == 27){
        System.out.println("n war 1");
             }
             else{
                 System.out.println ("n war nicht 1");
             }
        
    }
}