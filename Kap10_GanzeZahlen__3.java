public class Kap10_GanzeZahlen {

    public static void main(String [] args) {
        
        int i = 0123; // Oktalzahl, dezimaler Wert 1*8*8+2*8+3=83
        System.out.println(i);
        int j = 0x100; // Hexadezimalzahl, Wert 1*16*16=256
        System.out.println(j);
        int k = 0xAB; // Hexadezimalzahl, 10*16 + 11 = 171
        System.out.println(k);
        int b = 0b1001; // Binaerzahl, 1*2*2*2+1 = 9
        System.out.println(b);
        int l = 123_456_789; // neunstellige Dezimalzahl, _ zur Uebersichtlichkeit
        System.out.println(l);
        
        //Umrechnung
        int m = Integer.parseInt("100", 16);    // 1 * 16 * 16 = 256
        System.out.println(m); 
        int n = Integer.parseInt("100", 2);     // 1 * 2 * 2 = 4
        System.out.println(n);
        
        //Umrechnung mit Strings
        String s;
        s = Integer.toString(160, 16);  //liefert 160 als Hexadezimalzahl
        System.out.println(s);
        s = Integer.toString(9, 2);     // liefert 9 in Binaerschreibweise
        System.out.println(s);
        s = Integer.toString(35, 36);   // liefert 'z'
        System.out.println(s);
    }
}