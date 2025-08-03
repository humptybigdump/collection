/* Demoprogramm: numerische Berechnung von "pi" (Kreiskonstante)  

   Version 3b: [aus BAUINFO I, WS14/15, V4]

               Integration mit der Mittelpunktsregel
               & Anpassung der Interallanzahl "N" in 
                 aeusserer "while" Schleife

    Bauinfo II, SS24, V3/V4 (28.5.2024):
    
    * objektorientiert
      - mit Ausgabe in Datei [& Destruktor] 
      - Elementfunktionen 'const' 
      - Beispiel einer const-Elementvariablen ('NUM')
         -> erfordert Initialisierungsliste in Konstruktor
                                                       */

#include <iostream>
#include <fstream>  
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
  ofstream file;    //Objekt zur Ausgabe in Datei
  void write();
  const int NUM;    //konstante Elementvariable [Beispiel]
public:
  void show();
  void setNum(int); //Setzen der Integrationsparameter
  numInt(); // Standardkonstruktor
  numInt(double a, double b); // Konstruktor #2
  ~numInt(); // Destruktor
  double getError() const;
};

int main()
{
  //numInt IVec[10]; // Feld mit 10 Objekten vom Typ "numInt"
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
numInt::numInt(): NUM(1) {
  cout << "Konstruktor 1! " << endl;
  lGrenze=0.0;
  rGrenze=1.0;
  N=10;
  file.open("pi_out.txt");
}
numInt::numInt(double a, double b): NUM(int(a)) {
  cout << "Konstruktor 2! " << endl;
  lGrenze=a;
  rGrenze=b;
  N=10;
}
numInt::~numInt(){  //Destruktor
  cout << "Destruktor!" << endl;
  file.close();
}
void numInt::setNum(int n){
  //hier fehlen noch Filter!
  N=n;
  return; 
}
void numInt::write(){
  cout << "[writing]" << endl;
  file << N << " " << I << " " << err << endl;
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
  // Ausgabe in Datei:
  write();
  //this->write(); //alternative Moeglichkeit

  return; 
}
double numInt::setError(double refval){
  err = fabs(I-refval)/refval;
  return err ;
}
double numInt::getError() const{
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
