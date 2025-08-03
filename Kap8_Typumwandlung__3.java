public class Kap8_Typumwandlung {
    public static void main(String args[]) {
      
    double d = 12.89;
    int i = (int) d; //Typecast
    System.out.println (i);

    // genauso fuer negative Zahlen
    d = -12.89;
    i = (int) d; // wird nicht abgerundet, sondern abgeschnitten!
    System.out.println (i);
    
    
    //Mit boolean Werten indirekte Umwandlung moeglich
    boolean b;
    int j = 2;
    boolean b1 = true;
    int k;
    //Von int zu boolean
    // wandelt 0 in false, 1 (oder eine beliebige Zahl ungleich 0) in true
    b = j != 0; 
    System.out.println (b1);
    
    //Von boolean zu int
    // wandelt false in 0, true in 1
    k = b1 ? 1 : 0; 
    System.out.println (k);
    
    //Bei gemischten Ausdruecken wird im groesseren Typ gerechnet 
    int m = 1;
    double l = 1;
    l = l + m;  //Funktioniert da l vom groesseren Typ ist
    //m = l + m; // Fehler, m ist von kleinerem Typ
    
    //Mit byte
    byte o = 1, p = 2;
    // byte q = o + p; // Fehler! Summe ist int
    byte q = (byte) (o + p); // mit Typecast geht es
    
    //Komb. Zuweisung
    byte r = ++p; // okay, Ausnahme ++ und --
    // r = r + o; // Fehler! Summe ist int
    r += o; // okay, unterschiedliche Auswertung
    q += 500; // okay, da nur eine Auswertung stattfindet

    

    }
}