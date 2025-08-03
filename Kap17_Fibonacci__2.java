public class Kap17_Fibonacci{
    
    //Methode zur Berechnung der i-ten Fib.-Zahl 
    static int fibonacci (int i) {
    if (i <= 1)
        return 1;   // nicht rekursiver Zweig
    else
        return fibonacci(i-1) + fibonacci(i-2);     // zwei(!) rekursive Aufrufe
}
    
    public static void main (String [] args){
        
        int i = 5;
        //Aufruf zur Methode
        double f = fibonacci (i);
        System.out.println ("Die " + i + "-te Fibonacci-Zahl ist " + f);
    }
}