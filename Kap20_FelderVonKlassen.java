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
public class Kap20_FelderVonKlassen {
    
    public static void main(String args[]) {
        
        // Feld mit 4 Referenzen auf Person erzeugen, es werden noch keine Instanzen erzeugt
        Person[] familie = new Person[4];

        // 4 Instanzen von Person erzeugen,
        for (int i = 0; i < 4; i++)
            familie[i] = new Person();

        // jetzt ist Zugriff auf Instanzen moeglich, wenn nicht anders angegeben automatisch mit 0 initialisiert
            familie[2].vorname = "Hannah";
            System.out.println (familie[1].alter);

    }
}
