public class Student extends Person {
   private static int sanzahl = 0;               // Klassenvariable
   int matrikel;                                 // Instanzenvariablen
   public Student() {                            // Standardkonstruktor
      super();                                   // Person()
      sanzahl++;                                 // Instanzenzaehler
   }
   public Student(String n, String v, int a, int m) { // Konstruktor (4 Arg)
      this();                                    // Aufruf Standardkonstruktor
      this.name = n; this.vorname = v;
      this.alter = a; this.matrikel = m;
   }
   public Student(Student anderer) {             // Kopier-Konstruktor
      this(anderer.name, anderer.vorname, anderer.alter, anderer.matrikel);
   }
   public String toString() {                    // Ueberladen toString
      return name + ", "+ vorname + " (" + matrikel + ")";
   }
   public static int getAnzahl() {               // Klassenmethode
      return sanzahl;
   }
   // MAIN
   public static void main(String [] s) {
      Person max = new Person("Maier","Max",21); // Konstruktor (3 Arg)
      Student max2 = new Student("Mayer","Max",24,1234); // Konstruktor (4 Arg)
      System.out.println(max);                   // Maier, Max (21)
      System.out.println(max2);                  // Mayer, Max (1234)
      System.out.println(Person.getAnzahl());    // 2
      System.out.println(Student.getAnzahl());   // 1
   }
}
