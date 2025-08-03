public class Kap9_Reihenfolge {
    public static void main(String args[]) {
    
    int x = 21, y = 12, hilf;
    
    //Sortiere so, dass y die groessere Zahl ist
    if (x > y) {    //Klammerung unbedingt notwendig, da mehrere Anweisungen folgen
        hilf = x;   //der Wert von x wird in Hilfsvar. gespeichert
        x = y;  //y wird x zugeschrieben. x hat dann keine Info mehr ueber uerspruenglichen Wert
        y = hilf;     //Deshalb y den in Hilfsvar. gespeicherten Wert von x zuschreiben
        }
    System.out.println (x + " " + y);   //Ausgabe in aufsteigender Reihenfolge
    }
}