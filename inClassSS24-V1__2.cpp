/* Demoprogramm: numerische Berechnung von "pi" (Kreiskonstante)  

   Version 3b: [aus BAUINFO I, WS14/15, V4]

               Integration mit der Mittelpunktsregel
               & Anpassung der Interallanzahl "N" in 
                 aeusserer "while" Schleife

    Bauinfo II, SS24, V3 (7.5.2024):
    * objektorientiert
    * fehlt noch:
      - Ausgabe in Datei [& Destruktor]
    * Fragen:
      (i)   welche Elementfunktionen sollten 'const' sein?
      (ii)  wie geht man mit 'const' Elementdaten um? Wo werden diese
            initialisiert? [Bsp.: Integrationsgrenzen]
      (iii) Wie definiert/initialisiert/nutzt man Felder von Objekten?
                                                       */

#include <iostream>  
#define _USE_MATH_DEFINES /* MS Visual Studio */
#include <cmath>  
using namespace std;

class numInt{
private:
  double lGrenze,rGrenze; //Integrationsgrenzen
  int N; //Anzahl Stuetzstellen
  double dx; //Breite Intervallanteile
  double I;  //approx. Wert des Integrales
  double err; // Fehler der Approx. 
  void compute();
  double setError(double refval);
public:
  void show();
  void setNum(int); //Setzen der Integrationsparameter
  numInt(); // Standardkonstruktor
  numInt(double a, double b); // Konstruktor #2
  double getError();
};

int main()
{
  numInt pi;
  int N=10;  //Startwert fuer die Anzahl Stuetzstellen

  double err_old,err=1e5;
  double toleranz=1e-8; //Fehlertoleranz

  while (err>toleranz){
    pi.setNum(N);
    pi.show();

    err=pi.getError();
    err_old=err;
    N=N*2;
  }

  return 0; 
}
numInt::numInt(){
  cout << "Konstruktor 1! " << endl;
  lGrenze=0.0;
  rGrenze=1.0;
  N=10;
}
numInt::numInt(double a, double b){
  cout << "Konstruktor 2! " << endl;
  lGrenze=a;
  rGrenze=b;
  N=10;
}
void numInt::setNum(int n){
  //hier fehlen noch Filter!
  N=n;
  return; 
}
void numInt::show(){
  compute(); // Möglichkeit 1
  //this->compute(); // aequivalente Möglichkeit 2

  double pi_ref=M_PI; 
  setError(pi_ref);
  
  // Ausgabe folgt:
  cout << "N=" << N << endl;
  cout.precision(17);  //Einstellung: Ausgabe mit 17 Nachkommastellen
  cout << "pi (approx) = " << I << endl;
  cout << "pi (ref)    = " << pi_ref << endl;
  cout.scientific;   // Umstellen auf  Ausgabe in wissenschaftlicher Notation
  cout << "rel. Fehler = " << err << endl;

  return; 
}
double numInt::setError(double refval){
  err = fabs(I-refval)/refval;
  return err ;
}
double numInt::getError(){
  return err ;
}
void numInt::compute(){
  I = 0.0;
  double xi,g_xi; 
  
  dx=(rGrenze-lGrenze)/N;
  for(int i=1;i<=N;i++)
    {
      xi=lGrenze+(i - 0.5)*dx;
      g_xi=4.0/(1.0+xi*xi);
      I =  I + g_xi * dx; 
    }
  return; 
};
