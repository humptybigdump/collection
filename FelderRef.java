public class FelderRef {
  public static void main(String[] args) {
    // Felder sind kontainer Datentypen, (keine primitiven Datentypen)
    // Felder sind vom type Referenz
    char [] original = {'a','b','c'};
    char [] kopie = original;
    original[1] = 'x';
    System.out.println(kopie[1]);
    // Primitive Datentypen werden kopiert!
    char c = 'a';
    char d = c;
    c = 'x';
    System.out.println(d);
  }
}
