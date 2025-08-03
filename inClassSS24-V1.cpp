/* Kalenderprogramm
   Bauinfo 2, V1, 17.4.2023
   
   Version 1: beutzerdefinierte Datenstruktur "datum"
              (nicht objektorientiert)

   fehlt:   weitere Funktionen (aus Zeitgruenden)
            Integration Daten/Funktionen in Objekte (OOP)
*/
#include <cmath>
#include <iostream>
using namespace std;

struct datum{
  //Datenstruktur zur Speicherung eines Tagesdatums
  // Triplet aus Ganzzahlen
  int day;
  int month;
  int year;
};
void dmy_konverter(datum & x);
void set_datum(int d, int m, int y, datum & x);

int main( ) {
  datum heute;
  cout << sizeof(int) << endl;
  cout << sizeof(datum) << endl;
  cout << sizeof(heute) << endl;

  // heute.day=-22; //<-sollte nicht moeglich sein!
  
  set_datum(160,4,2024,heute);

  cout << &heute << endl;
  
  dmy_konverter(heute);
  
  return 0;
  }
void set_datum(int d, int m, int y, datum & x){
  if(d>=1 && d<=31){
    x.day=d;
  }else{
    cout << "ERROR: Tag " << d << endl;
    exit(EXIT_FAILURE);
  }
  x.month=m;
  x.year=y;
  return ;
}
void dmy_konverter(datum & x){
  cout << &x << endl;
  
  cout << "Datum: " << x.day <<"."<< x.month<<"."<<x.year<<endl;
  return; 
}
