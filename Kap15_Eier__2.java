public class KAp15_Eier {

    public static void main (String [] args){
       
       // "Einfache" Methode:
        int i=0;
        //Probiere jedes i, beginnend mit 0...
        do {
            i++;
        }
        //...bis man eine Zahl findet, die bei jeder Division Rest 1 laesst
        while 
        (i%2!=1 || i%3!=1 || i%4!=1 || i%5!=1 || i%6!=1 || i%7!=0);
        //Ergebnis:
        System.out.println("Es waren mindestens " + i + " Eier im Korb.");
        
        
        //Methode nach weiteren Ueberlegungen:
        //Beginne mit i = 1, teste dann alle Summen, auf die Vielfache von 60 addiert wurden
        //Damit muessen nicht mehr alle Zahlen getestet werden
        i=1;
        do {
            i += 60;
             }
        while 
        (i%7 != 0); //Da weder der Modulo-Operator, noch der Vergleich Rechenzeit benoetigen, gibt es hier keinen Unterschied zu oben
        //Liefert gleiches Ergebnis:
        System.out.println("Es waren mindestens " + i + " Eier im Korb.");
        
        /*Wenn man nun eins der beiden Programmteile auskommentiert sieht man, 
        dass sie praktisch gesehen die gleiche Rechenzeit benoetigen. 
        In diesem Fall lohnen sich die Ueberlegungen also nicht. 
        Es gibt aber Rechnungen, wo solche mathematischen Tricks enorm viel 
        Zeit und Rechenleistung sparen koennen!!
        */
        
    }
}
