/**
 * Hollywood-Klasse, die das Zusammenspiel der Klassen Serie, Charakter und
 * Schauspieler demonstriert
 * 
 * @author prog1-Team
 * @version 1.0
 */

public class Hollywood {
    /**
     * Hauptprogramm.
     * 
     * @param args Kommandozeilenarguemnte (hier unverwendet)
     */
    public static void main(String[] args) {
        Serie bb = new Serie("Breaking Bad", 3);
        Serie lost = new Serie("LOST", 5);
        Serie mf = new Serie("Modern Family", 3);

        // Demonstriere eine Serie///
        Schauspieler bryan = new Schauspieler("Bryan Cranston");
        Schauspieler aaron = new Schauspieler("Aaron Paul");
        Schauspieler anna = new Schauspieler("Anna Gunn");

        Charakter walter_white = new Charakter("Walter", bb, bryan);
        Charakter jesse = new Charakter("Jesse", bb, aaron);
        Charakter skyler = new Charakter("Skyler", bb, anna);

        System.out.println(bb);

        // Weitere Serien//
        System.out.println(lost);
        System.out.println(mf);

    }
}
