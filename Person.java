public class Person {
   private static int anzahl = 0;                // Klassenvariable
   int alter;                                    // Instanzenvariablen
   String name, vorname;
   public Person() {                             // Standardkonstruktor
      anzahl++;                                  // Instanzenzaehler
   }
   public Person(String name, String vorname, int alter) { // Konstruktor (3 Arg)
      this();                                    // Aufruf Standardkonstruktor
      this.name = name; this.vorname = vorname;
      this.alter = alter;
   }
   public Person(Person anderer) {               // Kopier-Konstruktor
      this(anderer.name, anderer.vorname, anderer.alter);
   }
   public String toString() {                    // Ueberladen toString
      return name + ", "+ vorname + " (" + alter + ")";
   }
   public static int getAnzahl() {               // Klassenmethode
      return anzahl;
   }
   // MAIN
   public static void main(String [] s){
      Person max = new Person("Maier","Max",21); // Konstruktor (3 Arg)
      Person max2 = new Person(max);             // Kopier-Konstruktor
      max2.name = "Mayer"; max2.alter = 24;      // Editiere max2
      System.out.println(max);                   // Maier, Max (21)
      System.out.println(max2);                  // Mayer, Max (24)
      System.out.println(Person.getAnzahl());    // 2
   }
}