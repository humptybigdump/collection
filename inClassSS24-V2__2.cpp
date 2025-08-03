/* Kalenderprogramm
   Bauinfo 2, V2, 23.4.2024
   
   Version 2: beutzerdefinierte Datenstruktur "datum"
              (jetzt: objektorientiert)

   fehlt:   weitere Funktionen (aus Zeitgruenden)
*/
#include <cmath>
#include <iostream>
using namespace std;

class datum{
private:
  //Datenstruktur zur Speicherung eines Tagesdatums
  // Triplet aus Ganzzahlen
  int day;
  int month;
  int year;
  // Elementfunktionen:
public:
  void set_datum(int d, int m, int y);
  void display_datum();
  // (expliziter) Konstruktor
  datum();
  // (expliziter) Destruktor
  ~datum();
};

int main( ) {
  datum heute;
  {
    datum morgen; //dieses Objekt hat nur bis zur folgenden Klammer Gueltigkeit 
  }
  cout << sizeof(int) << endl;
  cout << sizeof(datum) << endl;
  cout << sizeof(heute) << endl;

  //heute.day=-22; //<-jetzt nicht mehr moeglich!
  
  heute.set_datum(23,4,2024);

  cout << &heute << endl;
  
  heute.display_datum();
  
  return 0;
  }
datum::datum(){
  // Implementierung eines Konstruktors
  cout << "Konstruktor aufgerufen!" << endl;
  return ;
}
datum::~datum(){
  // Implementierung eines Destruktors
  cout << "Destruktor aufgerufen!" << endl;
  return ;
}
void datum::set_datum(int d, int m, int y){
  if(d>=1 && d<=31){
    day=d;
  }else{
    cout << "ERROR: Tag " << d << endl;
    exit(EXIT_FAILURE);
  }
  month=m;
  year=y;
  return ;
}
void datum::display_datum(){
  //cout << &x << endl;
  
  cout << "Datum: " << day <<"."<< month<<"."<< year<<endl;
  // aequivalent: nutzen des 'this' Zeigers (sh. V5ff)
  //cout << "Datum: " << this->day <<"."<< this->month<<"."
  //    << this->year<<endl;
  return; 
}
