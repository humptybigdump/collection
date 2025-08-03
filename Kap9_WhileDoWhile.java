public class Kap9_WhileDoWhile {

    public static void main(String [] args) {
        
        //do-while-Schleife
        int n = 5;
        do{
            n /= 2; // Integerdivision
        }
        while (n >= 10); // Ueberprueft, ob weitere Schleifendurchlaufe stattfinden
        System.out.println (n);

        //Beispiel mit while-Schleife
        int m = 5;
        while (m >= 10){
            m /= 2;     // Ueberprueft zuerst ob Bedingung erfuellt ist, Anweisung wird nicht ausgefuehrt 
        }
        System.out.println (m);

    }
}