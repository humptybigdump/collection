/*
  Partikelsimulation
  (Version ohne Grafik)

  Entworfen in der Vorlesung Bauinfo-1, V6, am 12.12.2024

 */
#include <cmath>
#include <iostream>
using namespace std;

// globale Variablen:
const float R = 1.0; //Radius des Kreises
const float L = 100*R; // Seitenlaenge des Spielfeldes
const float DT=1.0 ;    //Zeitschritt
const float SPEED=L/(10*DT); //Geschwindigkeitsbetrag
const int NSTEP=20;   //Anzahl Schritte

void anfangswerte(float & x, float & y, float & u, float & v);
void bewegung(float & x, float & y, float u, float v);
void wandkollision(float x, float y, float & u, float & v);
int main( ) {
  float x,y;  //Zentrumsposition (Komponenten)
  float u,v;  //Geschwindigkeitsvektor

  anfangswerte(x,y,u,v);

  for(int i=0; i<NSTEP ; i++){
    bewegung(x,y,u,v);
    wandkollision(x,y,u,v);
    //Anzeige: [Ersatz fuer Grafik]
    cout << "(x,y,u,v) = " << x << " " << y << " " << u << " " << v << endl;
  }
  
  return 0;
  }
void wandkollision(float x, float y, float & u, float & v){
  if(x<R || x>(L-R)){
    u = -u;
  }
  if(y<R || y>(L-R)){
    v = -v;
  }
  return;
}
void anfangswerte(float & x, float & y, float & u, float & v){
  //BEM: 'call-by-reference' fuer alle 4 Parameter
  // -> aendert Wert der Variablen in aufrufender Funktion
  x=L/2;
  y=L/2;
  float alpha=M_PI/2.0;
  u=cos(alpha)*SPEED;
  v=sin(alpha)*SPEED;
  return ;
}
void bewegung(float & x, float & y, float u, float v){
  x = x + u*DT;
  y = y + v*DT;
  return;
}
