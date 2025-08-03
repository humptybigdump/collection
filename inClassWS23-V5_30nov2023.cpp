/* entwickelt in der Vorlesung "Bauinfo 1", V5, 30. Nov 2023 */
#define _USE_MATHDEFINES  /* MS Visual Studio */
#include <cmath>
#include <iostream>
using namespace std;

// Funktionsdeklarationen ("Prototypen")
double funktion1(double x );
void ableitung(double x, double & df, double & d2f);
int main( ) {
  double x, df, d2f, df_exakt, d2f_exakt;
  cout << "Eingabe von x (reelle Zahl): "<< endl;
  cin >> x;
  //
  ableitung(x,df,d2f);
  //
  cout.precision(12);
  cout << "f'        = " << df << endl;
  df_exakt=M_PI*cos(M_PI*x);
  cout << "f'_exakt  = " << df_exakt << endl;
  cout << "f''       = " << d2f << endl;
  d2f_exakt=-pow(M_PI,2)*sin(M_PI*x);
  cout << "f''_exakt = " << d2f_exakt << endl;
  return 0;
  }
// Funktionsdefinitionen
double funktion1(double x ){
  return sin(M_PI*x);
}
void ableitung(double x, double & df, double & d2f){
  double dx=1.e-5;
  double x1=x-dx;
  double x2=x+dx;
  double f1=funktion1(x1);
  double f=funktion1(x);
  double f2=funktion1(x2);
  df= (f2-f1)/(2*dx);
  d2f=(f1-2*f+f2)/(dx*dx);
  return; 
}
