/* 
   Demoprogramm zum Umgang mit Datenfeldern in C++
   
   Statistik eines Datenvektors: Mittelwert, ...

   --- Version mit zweidimensionalem Feld ---

   Entworfen in der Vorlesung Bauinfo-1, V7, am 19.12.2024

*/
#include <cmath>
#include <iostream>
using namespace std;

const int N1=3;
const int N2=2;

//void statistik(const float mat [][N2], float & mean, float & var);//<-auch OK
void statistik(const float mat [N1][N2], float & mean, float & var);

int main( ) {
  //definiert ein 2-D Feld 'mat'
  float mat[N1][N2];

  float mean=0.0;
  float var = 0.0;
  
  //Initialisierung in Schleife:
  for(int i=0; i<N1 ; i++){
    for(int j=0; j<N2 ; j++){
      mat[i][j]=i+j;
      cout << " i,j = " << i << "," << j << " m[i,j]=" << mat[i][j] << endl;
    }
  }
  //Berechnung der Statistik:
  statistik(mat,mean,var);
  cout << "mean = " << mean << endl;
  cout << "var = " << var << endl;
  return 0;
  }
//void statistik(const float mat [][N2], float & mean, float & var){
void statistik(const float mat [N1][N2], float & mean, float & var){
  mean = 0.0;
  var = 0.0;
  for(int i=0; i<N ; i++){
     for(int j=0; j<N2 ; j++){
   mean=mean+mat[i][j];
  }
  }
  mean=mean/float(N1*N2);
  for(int i=0; i<N ; i++){
      for(int j=0; j<N2 ; j++){
  var = var + pow(mat[i][j] - mean,2);
  }
  }
  var = var / float(N1*N2);

  return ; 
}
