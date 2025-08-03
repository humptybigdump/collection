/* Demoprogramm zur Geometrie in der Ebene
   zu "Bauinfo II", V4, SS2024, 4.6.2024
   
   aus Vorlage: 'vorlage_selbst_shape.cpp'
   
   Klassen - Vererbung

   'shape' ist abstrakte Basisklasse
   'kreis'   wird von 'shape' abgeleitet
   
   Aufgabe:
   * implementieren Sie die abgeleitete(n) Klasse(n)

   HIER:
   * erledigt für eine Kindklasse 'kreis'
   
 */
#include <cmath>
#include <iostream>
using namespace std;
struct point{
  float x;
  float y;
};
class shape{//abstrakte Basisklasse
protected:
  point position;//Referenzpunkt
public:
  shape(); //Konstruktor
  shape(float a, float b); //Konstruktor #2
  virtual void anzeigen() =0; //virtuelle Methode
  
};
class kreis: public shape{
private:
  float radius;
  // 'position' ist der Mittelpunkt (Basisklasse)
public:
  void anzeigen();// "Pflicht"!
  kreis(); //Konstruktor
  kreis(float a, float b, float c); //Konstruktor #2
};
int main( ) {
  // shape s1; // illegal!
  kreis k1;
  k1.anzeigen();
  kreis k2(2.5,3.7,22.45);
  k2.anzeigen();
  return 0;
  }
//[im Folgenden: fehlende Implementierungen...]

kreis::kreis(){
  cout << "kreis Konstruktor!" << endl;
  radius = 1.0;
}
kreis::kreis(float a, float b, float c):
  shape(a,b)
{
  cout << "kreis Konstruktor #2!" << endl;
  radius = c;
}
shape::shape(float a, float b){
  cout << "shape Konstruktor #2!" << endl;
  position.x=a;
  position.y=b;
}
shape::shape(){
  cout << "shape Konstruktor!" << endl;
  position.x=0.0;
  position.y=0.0;
}
void kreis::anzeigen(){
  cout << " Kreis: Mittelpunkt=" <<position.x<<" , " <<  position.y ;
  cout << " Radius=" << radius << endl;
  return;
}
