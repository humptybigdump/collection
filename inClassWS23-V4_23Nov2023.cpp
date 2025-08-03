/* Beispielprogramm Bauinfo 1, VL4 - Schleifen (23.11.2023)
   numerische Integration -> Kreiskonstante 'pi'
 */
#define _USE_MATHDEFINES //<-notwendig fuer VisualStudio (M_PI)
// "cmath" enthaelt die Konstante "M_PI"
#include <cmath>
#include <iostream>
using namespace std;

int main( ) {
  double tolerance = 1.e-8;
  double err=1.0;
  int N=10;  //Startwert
  double pi;
  double dx;
  double xi, g;
  int j=0; //<-dient als Zaehler der durchlaeufe der aeusseren Schleife
  while(err > tolerance){// aeussere Schleife (Anzahl Stuetzstellen N):
    j++;
    pi = 0.0;
    dx = 1.0 / double(N); // auch OK: dx=1.0/N; //nicht OK:dx=1/N;
    
    // (innere) Berechnungs-Schleife:
    for(int i=1; i<=N; i++){
      //cout << " Schleife: i=" << i << endl;
      xi=(i-0.5)*dx;
      g= 4./(1+xi*xi);   //oder: g= 4./(1+pow(xi,2));
      pi = pi + g*dx;
    }
    cout.precision(15); //<-Ausgabe ab jetzt mit 15 Nachkommastellen
    cout << "N=" << N << ": pi=" << pi << endl;
    cout << "Referenzwert pi=" << M_PI << endl;
    err=pi-M_PI;
    cout << "Fehler = " << err << endl;
    // Erhöhung von N (aeussere Schleife)
    N=N*2;
  }
  cout << "aeussere Schleife ist ... mal durchgelaufen: " << j << endl;
  return 0;
}
