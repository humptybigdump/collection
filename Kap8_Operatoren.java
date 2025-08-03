public class Kap8_Operatoren {
    public static void main(String args[]) {
      
      //Kombinierte Zuweisungsoperatoren
      int g = 1;
      int h = 1;
      
      g = g + 5; // ergibt g = 6
      h += 5;    //gleiches Ergebnis, ist aber schoenere Variante, spart eine Auswertung ein
      System.out.println ("g = " + g + ", h = " + h);
      
      //Reihenfolge
      int k = 2, l = 3, m = 4;
      k = l = m;    //Zuweisung von rechts nach links!
      System.out.println ("k = " + k + ", l = " + l + ", m = " + m);

      
      //Logische Operatoren
      //Pruefen ob x dezimale Ziffer ist, wenn ja erfolgt Ausgabe
      double x = 5;
      if(x >= 0 && x <= 9){
          System.out.println ("x ist dezimale Ziffer");
      }
      //Pruefen, ob x positive oder gerade Zahl ist, wenn ja erfolgt Ausgabe
      if (x > 0 || x%2 == 0){
          System.out.println ("x ist positive oder gerade Zahl");
      }
      //Pruefen, ob c ein Kleinbuchstabe ist, wenn ja erfolgt Ausgabe
      char c = 'b';
      if (c >= 'a' && c <= 'z' ){
          System.out.println (c + " ist ein Kleinbuchstabe");
      }
    
    }
}