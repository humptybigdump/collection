class Person {
    
    //Instanzvariablen
    private int alter; // geschuetzt vor unberechtigtem Zugriff
    public String vorname, name;
    
    // Methode, um alter abzufragen
    public int getAlter() { 
        return alter; // alter wird zurueckgegeben "obwohl" es private ist
        }

    // Konstruktor, ordnet uebergebene Var. den Instanzvar. zu
    Person(String vor, String nachname, int alt) {
        alter = alt;
        name = nachname;
        vorname = vor;
        }
}

//public class, diese muss main-Methode enthalten
public class Kap21_Zugriffe {
    
    public static void main(String args[]) {
        
        //Person mit Kostruktor erzeugen, fordert autom. Festlegung gewisser Var.
        Person p = new Person("Karl", "Maier", 19);

        // Zugriff auf Methode ist moeglich
        System.out.println(p.getAlter());

        // Zugriff auf private-Var. in anderer Klasse ist nicht moeglich
        // System.out.println(p.alter);

        // Zugriff auf public_Var. von anderen Klassen ist moeglich
        System.out.println(p.vorname);
        
    }
}