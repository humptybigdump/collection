public class Kap9_LeereAnweisung {

    public static void main(String [] args) {
        
        if (2 > 3);
        System.out.print("nanu"); // if-Anweisung wurde mit ; beendet, Ausgabe erfolgt unabhaengig von Wahrheitswert

        if (2 < 3)
            System.out.print("klar...");    //Ausgabe abhaengig von Wahrheitswert
        else;   // else-Anweisung wurde mit ; beendet 
        System.out.print("nanu");   //Ausgabe erfolgt also immer 
        
        int i = 33;
        for (i = 0; i < 10; i++);   //Schleife erhoeht nur i, fuehrt keine Anweisung aus, wird mit ; beendet
        System.out.print(i);    // Ausgabe erfolgt immer genau einmal
        
        /*Tipp:
        Setzte immer die geschwungenen Klammern
        Sie koennen auch leer stehen oder nur um eine einzelne Anweisung.
        */
        
    }
}