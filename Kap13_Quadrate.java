public class Kap13_Quadrate {

   static int[] quadrate(int dim) {
    
       // Feld der Laenge dim erzeugen
        int[] f = new int[dim];
        //Komponenten vom Feld festlegen
        for (int i = 0; i < dim; i++){
            f[i] = i * i; // Feldelemente: Quadratzahlen
        }
        return f;
        // hier endet die Lebensdauer der Referenz f, aber das Feld selbst existiert weiter
        // (in diesem Beispiel spaeter unter Namen: feld)
        }

    public static void main(String[] args) {
        int[] feld; // nur Referenz
        feld = quadrate(5); // Feld per Methode erzeugen
        
        // alle Feldelemente ausgeben
        for (int i = 0; i < feld.length; i++){
            System.out.println(feld[i]); 
             }
     }
}