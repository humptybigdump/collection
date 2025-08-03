/*
   Demoprogramm zum Umgang mit Dateien in C++
   
   Entworfen in der Vorlesung Bauinfo-1, V8, am 18.1.2024

     -- SCHREIBEN eindimensionaler Felder --

 */
#include <cmath>
// 1. Ausgabebiblio laden
#include <fstream>
#include <iostream>
using namespace std;
const int N=2;

int main( ) {
  // 2. Objekt erzeugen:
  ofstream oFile;
  float xx;
  float  x[N], f[N];
  for(int i=0;i<N;i++){
    xx=float(i)/(float(N)-1);
    x[i]=xx;
    f[i]=exp(-xx*xx/0.1)*sin(20*xx);
    }
  // 3. Datei oeffnen:
  oFile.open("aus.txt");

  oFile << scientific;
  oFile << showpos;
  oFile.precision(5);
  // 4. eigentliches Schreiben:
  for(int i=0;i<N;i++){
    oFile << x[i] << " " << f[i] << endl;
    }
  // 5. Datei schliessen
  oFile.close();
  return 0;
}

