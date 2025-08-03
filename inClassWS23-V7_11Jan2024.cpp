/* 
   Demoprogramm zum Umgang mit Datenfeldern in C++
   
   Statistik eines Datenvektors: Mittelwert, ...

   Entworfen in der Vorlesung Bauinfo-1, V7, am 11.1.2024

*/
#include <cmath>
#include <iostream>
using namespace std;


void  init(float v[], int N); //Deklaration
float mean(const float v[], int N); //Deklaration

int main( ) {
  const int N=3;
  float vector[N];
  float mv;
  
  //Initialisierung des Vektors:
  init(vector,N);
  
  //Mittelwert:
  mv=mean(vector,N);
  
  cout << "Mittelwert = "<< mv << endl;
  
  return 0;
  }
void init(float v[], int N){
  for(int i=0; i<N; i++){
    v[i]=i*i;
    cout << "i=" << i << ": " << v[i] << endl;
  }

  return; 
}
float mean(const float v[], int N){
  //Mittelwert:
  float sum=0.0;
  for(int i=0; i<N; i++){
    //sum=sum+v[i];
    sum+=v[i];
  }
  sum=sum/float(N);
  return sum;
}
