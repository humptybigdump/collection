public class Kap18_SelectionSort{
    
    public static void main (String [] args){
        
        //Variablen
        double [] a = {1, 9, 6, 5, 8};
        int imin;
        double min;
        double temp;
        
        for (int i = 0; i <= a.length-2; i++) {// Minimum suchen
        min = a[i]; // zuerst ist a[i] das Minimum
        imin = i; // auch den Index i merken

        for (int j = i+1; j < a.length; j++){ // restl. a[j] pruefen
            if (a[j] < min) { // a[j] < bisheriges Minimum?
                min = a[j]; // Minimum speichern
                imin = j; // und auch sein Index
                }
        }
        
            // Elemente i und imin vertauschen
            temp = a[imin]; // ein Element zwischenspeichern
            a[imin] = a[i]; // dann das andere umspeichern
            a[i] = temp; // temp umspeichern
            } 
        
        //Ausgabe des sortierten Feldes
        for (int i = 0; i < a.length ; i++){
            System.out.print (a[i] + "  ");
        }
        
    }
}