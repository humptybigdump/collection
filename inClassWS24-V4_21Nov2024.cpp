/* Beispielprogramm Bauinfo 1, VL4 - Schleifen (21.11.2024)
   numerische Integration -> Kreiskonstante 'pi'
 */
#define _USE_MATH_DEFINES //notwendig in MS Visual Studio! (M_PI)
#include <cmath>
#include <iostream>
using namespace std;

const double tolerance = 1.e-8; //<-wissenschaftliche Notation

int main( ) {
  double pi =0, dx, g_xi, xi, error = 2*tolerance;
  cout.precision(12);
  cout << "M_PI = " << M_PI << endl;

  int N=4; //Anfangswert 
  
  // aeussere Schleife ('N' wird variiert)
  //while( error > tolerance ){   //<-kopfgesteuert
  do{                             //<-fussgesteuert

    pi =0.0;
    dx = 1.0/double(N); 
    for(int i=1; i<=N ; i++){
      xi=(i-0.5)*dx;        //<-aktuelle Stuetzstelle
      g_xi=4/(1+pow(xi,2)); //<-entspr. Funktionswert
      pi = pi + g_xi*dx;    //<-Aufaddieren
      //cout << "i=" << i << " xi= " << xi << " g = " << g_xi << endl;
    } //Ende innere Schleife
    
    cout << " N = " << N << endl;
    cout << "pi_approx = " << pi << endl;
    error = pi - M_PI;
    cout << scientific ;
    cout << "error     = " << error << endl;
    // Erhoehung von 'N' fuer naechsten Durchlauf
    N = N * 2;
  }
  while( error > tolerance );//<-fussgesteuert
    
  return 0;
  }
