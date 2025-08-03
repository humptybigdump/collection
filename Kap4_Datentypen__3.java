public class Datentypen {
    
    public static void main (String [] args) {
        
        
        //Ganzzahlige Datentypen
        int i = 1;
        // i = i + 20000000000;   //Waere Fehler, da Zahl fuer int zu gross
        long l = 1L;
        l = l + 20000000000L;   //Funktioniert da fuer long mehr Speicherplatz reserviert ist
        
        //Gleitkommazahlen
        double d = 5 * (10^(-5));   //Kennt ^-Operation nicht; fasst es als + auf
        double d1 = 5E-5;   //Richtige Notation
        System.out.println ("d = " + d + ", d1 = " + d1);   //Mit Ausgabe pruefen, was passiert ist
        
        //char
        char c = '\u0051';
        System.out.println (c);
        //char  ':' entspricht nach Typecast der Zahl 58
        char c1 = ':';
        System.out.println ((int) c1);

        
        //Strings
        String s = "ich";
        System.out.println ("Hiermit zeige " + s + ",dass es reicht, wenn " + s + " das Wort '" + s + "' nur einmal schreibe!");
        
        //bool
        boolean b = false;
        int j = 1;
        //Wenn j <= 3 wird Aussage auf 'true' gesetzt, sonst bleibt Wahrheitswert 'false'
        if (j <= 3){
            b = true;
        }
        
        System.out.println (b);
    }
}