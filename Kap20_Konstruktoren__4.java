//Klasse Person
class Person {
    
    //Instanzvariablen (werden fuer jede Instanz (Person) einzeln angelegt)
    String name, vorname;
    int alter;
    int jahr;

    //Standardkonstruktor
    Person (){
    }
    
        
    // 2. Konstruktor, der uebergebene Variable alter der Instanzvar. alter zuordnet
    Person(int alter) {
        this.alter = alter;
        }
    }

//Moeglichkeit hier noch mehr Klassen zu definieren


//public class, diese muss main-Methode enthalten
public class Kap20_Konstruktoren {
    
    //Hier koennen auch weitere Methoden stehen
    
    //main
    public static void main(String args[]) {
        
        //Erzeugen der Instanz mit Standardkonstruktor
        Person clara = new Person ();
        
        //belege Variablen von Clara
        clara.name = "Maier";
        clara.vorname = "Clara";
        
        // Alter wird automatisch mit 0 initialisert
        System.out.println ( clara.vorname + " " + clara.name + " ist " + clara.alter + " Jahre alt");
        
        //Alter angeben geht ueber:
        clara.alter = 18;
        System.out.println ( clara.vorname + " " + clara.name + " ist " + clara.alter + " Jahre alt");
        
        //Erzeugen der Instanz mit 2. Konstruktor
        Person tom = new Person (18);
        
        //belege Variablen von tom
        tom.name = "Maier";
        tom.vorname = "Tom";
        
        // Hierbei wurde Alter schon ueber das erzeugen mittels des Konstruktors festgelegt
        // Konstruktor fordert also automatisch die Angabe von verschiedenen Var.
        System.out.println ( tom.vorname + " " + tom.name + " ist " + tom.alter + " Jahre alt");

    }
}