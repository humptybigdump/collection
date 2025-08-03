/*
   Demoprogramm zum Umgang mit Dateien in C++
   
   Entworfen in der Vorlesung Bauinfo-1, V8, am 18.1.2024

     -- LESEN eindimensionaler Felder --

 */
#include <cmath>
// 1. Aus/Eingabebiblio laden
#include <fstream>
#include <iostream>
using namespace std;
const int N=2;

int main( ) {
  // 2. Objekt erzeugen:
  ifstream iFile;
  float xx;
  float  x[N], f[N];

  // 3. Datei oeffnen:
  iFile.open("aus.txt");

  // 4. eigentliches Lesen
  for(int i=0;i<N;i++){
    iFile >> x[i];
    iFile >> f[i];
    cout << "i=" << i << " x=" << x[i] << " f=" << f[i] <<endl;
    }
  // 5. Datei schliessen
  iFile.close();
  return 0;
}

