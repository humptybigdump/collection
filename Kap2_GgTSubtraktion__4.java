public class Kap2_GgTSubtraktion{
    
    public static void main (String [] args){
        
        //Lege positive Variablen fest
        int a = 20;
        int b = 24;
        
        //Solange a und b positiv, ziehe das Kleinere vom Größeren ab.
        while (a > 0 && b > 0) {
            if (a > b) {
                a = a-b;
            } else {
                b = b-a;
                }
        }
        
        //Wenn der Rest 0 ist, ist a ein Teiler von b
        if (b == 0) {
            System.out.println (a);
            } else {
                System.out.println (b);
                }
    }
}

