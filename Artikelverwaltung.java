import java.util.*;
public class Artikelverwaltung {
  static Scanner sc;

  /*Lese einen Artikel von der Konsole ein*/
  static Artikel liesArtikel () {
    Artikel ding = new Artikel();
    System.out.print( "Bitte Artikelname eingeben: " );
    ding.bezeichnung = sc.next();
    System.out.print( "Bitte vorhandene Einheiten eingeben: " );
    ding.anzahl = sc.nextInt();
    System.out.print( "Bitte Preis pro Einheit eingeben: " );
    ding.preis = sc.nextDouble();
    return ding;
  }

  /*Lese alle Artikel von der Konsole ein*/
  static Artikel[] liesListe ( int anzArtikel ) {
    Artikel[] artikelliste = new Artikel[anzArtikel];
    for ( int i = 0; i < anzArtikel; ++i ) {
      artikelliste[i] = liesArtikel();
    }
    return artikelliste;
  }

  /*Gebe einen Artikel auf der Konsole aus*/
  static void zeigeArtikel ( Artikel ding ) {
    double gesamtwert = ding.anzahl * ding.preis;
    System.out.printf( "%10.2f %10.2f %6s %20s \n",
                       gesamtwert, ding.preis, ding.anzahl,
                       ding.bezeichnung );
  }

  /*Gebe eine Artikelliste auf der Konsole aus*/
  static void zeigeListe ( Artikel[] artikelliste ) {
    System.out.printf( "%10s %10s %6s %20s \n",
                       "Gesamtwert", "Preis", "Anzahl",
                       "Bezeichnung" );
    double gesamtwert = 0.0;
    for ( int i = 0; i < artikelliste.length; ++i ) {
      zeigeArtikel( artikelliste[i] );
      gesamtwert +=   artikelliste[i].anzahl
                    * artikelliste[i].preis;
    }
    System.out.printf( "----------\n%10.2f\n", gesamtwert );
  }

  /*Hauptprogramm*/
  public static void main ( String[] args ) {
    Locale.setDefault( Locale.US );
    sc = new Scanner( System.in );

    System.out.print( "Bitte Anzahl der verschiedenen Artikel eingeben: " );
    int anzArtikel = sc.nextInt();

    /*Lese alle Artikel von der Konsole ein*/
    Artikel[] artikelliste = liesListe( anzArtikel );

    /*Gebe alle Artikel auf der Konsole aus*/
    zeigeListe( artikelliste );
  }
}
