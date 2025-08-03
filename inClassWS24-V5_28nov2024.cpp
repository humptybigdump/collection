/* entwickelt in der Vorlesung "Bauinfo 1", V5, 28. Nov 2024 */

#include <cmath>
#include <iostream>
using namespace std;

//float funktion1(float x);   //<- FunktionsDEKLARATION
//float funktion1(float);   //<- FunktionsDEKLARATION
float funktion1(float y);   //<- FunktionsDEKLARATION
float ableitung(float x0, float dx);  //<- FunktionsDEKLARATION

int main( ) {
  float x0 = M_PI/4.0;
  float dx = 1.e-5;
  float f1, f2, df, df_exact;
  // Aufruf der Funktion "funktion1"
  /* cout << "sin(0) = "    << funktion1(0.0) << endl;
  cout << "sin(M_PI/2) = " << funktion1(M_PI/2.0) << endl; 
  cout << "sin(M_PI) = " << funktion1(M_PI) << endl; */
  
  // Berechnung der Ableitung:
  df = ableitung( x0, dx);
  
  df_exact = cos(x0);
  cout << "x0 = " << x0 << endl;
  cout << "df(x0) = " << df << endl;
  cout << "df(x0)_ex = " << df_exact << endl;
  cout << "Fehler: " << abs(df-df_exact) << endl;
  
  return 0;
  }

// DEFINITION einer Funktion "funktion1"
float funktion1(float x0){  //<-Signatur der Funktion
  //float f=sin(x);
  //return f;
  return sin(x0);
};
// DEFINITION einer Funktion "ableitung"
float ableitung(float x0, float dx){  //<-Signatur der Funktion
  float f1 = funktion1(x0 - dx);
  float f2 = funktion1(x0 + dx);
  //float df = (f2-f1)/(2*dx);
  //return df ;
  return (f2-f1)/(2*dx);
};
