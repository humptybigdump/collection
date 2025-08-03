public class Kap2_GgT{

    public static void main (String [] args){ //ueblicher Rahmen
    
        //Setze Variablen, !gilt nur für positive Zahlen! (s.Kap. 6)
        int a = 20; 
        int b = 24;
        
        //Deklariere Variable h, noch keine Wertzuweisung
        int h;
        
        //Führe Schritte in {…} aus, solange b != (ungleich) 0
        while (b != 0){
           
            //Schritte 2-4 (s. 2.2)
            h = a % b;
            a = b;
            b = h;
            
            }
        
        //Gebe a als GgT auf der Konsole aus
        System.out.println ("Der GgT ist " + a);
        
    }
}
