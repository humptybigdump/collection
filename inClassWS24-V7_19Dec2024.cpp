/* 
   Demoprogramm zum Umgang mit Datenfeldern in C++
   
   Statistik eines Datenvektors: Mittelwert, ...

   Entworfen in der Vorlesung Bauinfo-1, V7, am 19.12.2024

*/
#include <cmath>
#include <iostream>
using namespace std;

const int N=5;

void statistik(const float vec [], float & mean, float & var);

int main( ) {
  //definiert ein 1-D Feld 'vec':
  //float vec[N] = { 1,2,3,4,6 }; //..mit Initialisierungsliste
  float vec[N];                   // ohne Initialisierung

  float mean=0.0;
  float var = 0.0;
  
  //Initialisierung in Schleife:
  for(int i=0; i<N ; i++){
    vec[i]=i*i;
    cout << " i = " << i << " vec[i]=" << vec[i] << endl;
  }

  //Berechnung der Statistik:
  statistik(vec,mean,var);
  cout << "mean = " << mean << endl;
  cout << "var = " << var << endl;
  
  return 0;
  }
void statistik(const float vec [], float & mean, float & var){
  mean = 0.0;
  var = 0.0;
  for(int i=0; i<N ; i++){
    mean=mean+vec[i];
  }
  mean=mean/float(N);
  for(int i=0; i<N ; i++){
    var = var + pow(vec[i] - mean,2);
  }
  var = var / float(N);

  return ; 
}
