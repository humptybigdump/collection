/* 
   Demoprogramm zum Umgang mit Datenfeldern in C++
   
   Statistik eines Datenvektors: Mittelwert, ...

      --- Version mit zweidimensionalem Feld ---

   Entworfen in der Vorlesung Bauinfo-1, V7, am 11.1.2024

*/
#include <cmath>
#include <iostream>
using namespace std;

  const int N1=2;
const int N2=3;


void  init(float v[][N2]); //Deklaration
float mean(const float v[][N2]); //Deklaration

int main( ) {
  float vector[N1][N2];
  float mv;
  
  //Initialisierung des Vektors:
  init(vector);
  
  //Mittelwert:
  mv=mean(vector);
  
  cout << "Mittelwert = "<< mv << endl;
  
  return 0;
}
void init(float v[][N2]){
  for(int i=0; i<N1; i++){
    for(int j=0; j<N2; j++){
      v[i][j]=i*i+j;
      cout << "i,j=" << i << ","<< j << ": " << v[i][j] << endl;
    }
  }
  return; 
}
  float mean(const float v[][N2]){
  //Mittelwert:
  float sum=0.0;
  for(int i=0; i<N1; i++){
    for(int j=0; j<N2; j++){
      //sum=sum+v[i];
      sum+=v[i][j];
    }
  }
  sum=sum/float(N1*N2);
  return sum;
}
