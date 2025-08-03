public class Kap18_BubbleSort{
    
    public static void main (String [] args){
        
        //s.Schaubild Kap. 18.3 (2)
        
        double [] a = {1, 7, 9, 4};     // Feld anlegen, das sortiert werden soll
        double temp;
        
        for (int i = 1; i < a.length; i++){     //Waehle im ersten Durchlauf 1. Element, im 2. Durchlauf das 3. usw
            for (int j = a.length-1; j >= i; j--){                //vergleiche das gewaehlte Element alle vom letzten rueckwaerts bis hin zum i-ten 
                if (a[j-1] > a[j]) {        //Wenn das davorrige Element groesser war...
                // ...vertausche a[j] und a[j-1] ueber Hilfsvar.
                temp = a[j];
                a[j] = a[j-1];
                a[j-1] = temp;
                    }
                } 
            } 
        //Damit werden die kleinsten Elemente nach vorne sortiert
        
        //Ausgabe des ueberarbeiteten Feldes mit Zahlen in gewuenschter Reihenfolge
        for (int i = 0; i < a.length ; i++){
            System.out.print (a[i] + "  ");
        }
    }
}