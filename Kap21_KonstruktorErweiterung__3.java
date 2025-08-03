//So darf eine Klasse mit Konstruktoren def. sein
class Person{
    
    String vorname;
    String name;
    int alter;
    
    //Konstruktor 1, mit this koennen Var. "gleich benannt" sein
    public Person(String vorname, String name){
        this.vorname = vorname;
        this.name = name;
        }
    
    // Konstruktor 2
    Person(String vorname, String name, int alter){
        this(vorname, name);    //da in 1. Konstruktor schon def.
        this.alter = alter;
        }
}

/*So duerfen Konstruktoren in einer Klasse nicht def. sein
class Person{
    
    String vorname;
    String name;
    int alter;
    
    // Konstruktor 1:
    Person(String vorname, String name){
        name = this.name; // Zuweisung von rechts nach links, ergibt keinen Sinn
        vorname = this.vorname; 
        }

    // Konstruktor 2:
    Person(int alter, String vorname, String name){
        this.vorname = vorname; // ok, ist 1. Anweisung
        this.name = name; // ok, ist 2. Anweisung des Konstruktors
        this(alter); // Will den ersten Konstruktor aufrufen, geht aber nur als erste Anweisung im Konstruktor
        } 
}
*/


//public class, diese muss main-Methode enthalten
public class Kap21_KonstruktorErweiterung {
    
    public static void main(String args[]) {
        
        //Verwendet Konstruktoren von nicht auskommentierter Klasse, die sind ja ok
        
        //Person mit 1. Kostruktor erzeugen
        Person p = new Person ("Tom" , "Maier");
        //Person mit 2. Konstruktor erzeugen
        Person p1 = new Person("Karl", "Maier", 19);

        // Zugriff auf Elemente erfolgt gleich, nur dass bei Tom nicht zwingend ein Alter angegeben wurde, wird dann autom. mit 0 initialisiert
        System.out.println(p.vorname + " ist " + p.alter + " Jahre alt.");
        System.out.println(p1.vorname + " ist " + p1.alter + " Jahre alt.");

    }
}