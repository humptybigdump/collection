//Klasse Person
class Person {
    
    //Instanzvariablen (werden fuer jede Instanz (Person) einzeln angelegt)
    String name, vorname;
    int alter;
    int jahr;


    /* Konstruktoren, Instanzmethoden und -variablen,
    // Klassenmethoden und -variablen
     Diese werden im weiteren Verlauf eingeführt und erläutert
    */
    }

//Moeglichkeit hier noch mehr Klassen zu definieren


//public class, diese muss main-Methode enthalten
public class Kap20_Personen {
    
    //Hier koennen auch weitere Methoden stehen
    
    //main
    public static void main(String args[]) {
        
        //Erzeugen der Instanzen (hier Personen) 
        Person clara = new Person();
        Person claus = new Person(); // initialisiert
        
        //belege Variablen von Clara
        clara.name = "Maier";
        clara.vorname = "Clara";
        
        //Zugriff ueber Instanzname.Variablenname
        System.out.println ( clara.vorname + " " + clara.name );
        
        //Jahr wird in Klasse Person automatisch mit 0 initialisiert
        System.out.println("Geburtsjahr:" + claus.jahr);
        
        int jahr; // nicht initialisiert
        //In main wird die Variable nicht automatisch initialisiert
        // System.out.println("Geburtsjahr: " + jahr);     //Ausgabe: Fehlermeldung

    }
}
