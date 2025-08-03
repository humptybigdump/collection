public class Kap12_MethodeZehnmal {
    
    //Methode, die das 10fache eines Wertes zurueckgibt
    static int zehnmal(int x) {
            return x * 10;
        }
    
    
        public static void main(String[] args) {
        
            // wiederholter Zugriff auf Methode
            int a = zehnmal (7);
            int b = zehnmal (3);
            System.out.println (a + " " + b);

    }
}