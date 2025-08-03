/*
   Demoprogramm zum Umgang mit Dateien in C++
   
   Entworfen in der Vorlesung Bauinfo-1, V8, am 18.1.2024

     -- SCHREIBEN ZWEI-dimensionaler Felder --

 */
#include <cmath>
// 1. Ausgabebiblio laden
#include <fstream>
#include <iostream>
using namespace std;
const int N1=2;
const int N2=3;

int main( ) {
  // 2. Objekt erzeugen:
  ofstream oFile;
  float f[N1][N2];
  for(int i=0;i<N1;i++){
  for(int j=0;j<N2;j++){
    f[i][j]=i*j;
    }
  }
  // 3. Datei oeffnen:
  oFile.open("aus2.txt");

  oFile << scientific;
  oFile << showpos;
  oFile.precision(5);
  // 4. eigentliches Schreiben:
  for(int i=0;i<N1;i++){
    for(int j=0;j<N2;j++){
      oFile << f[i][j] << " ";
    }
    oFile << endl;
  }
  // 5. Datei schliessen
  oFile.close();
  return 0;
}

