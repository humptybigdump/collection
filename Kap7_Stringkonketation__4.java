public class Kap7_Stringkonketation{
    
    public static void main (String [] args){
        System.out.println("Ergebnis:" + 1 + 2);    // + ist die Stringkonkat.
        System.out.println("Ergebnis:" + (1 + 2));  // + vor Klammer ist Stringkonkat, + in Klammer steht fuer Addition
        System.out.println(1 + ':' +2);    // Der char ':' entspricht dem int Wert 58 
                                            // (da char ein Datentyp ist)
        System.out.println(""+1+':'+2);    // Wegen "" wird der Ausdruck als String aufgefasst
        System.out.println(1+":"+2);    // ":" ist ein String

    }
}
