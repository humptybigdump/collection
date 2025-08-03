/*
  Partikelsimulation
  (Version MI Grafik)

  Entworfen in der Vorlesung Bauinfo-1, V6, am 14.12.2023

  BEM: hier werden weitere Funktionen und Bibliotheken benötigt
  (d.h. diese Datei alleine wird nicht kompilieren)
  -> sh. 'circle_win.zip' bzw. 'circle_linux.tgz'

 */
#include <cmath>
#include <iostream>
using namespace std;

const float R=1;   //Radius des Partikels
const float L=20;  //Spielfeldlbreite/-höhe
const float SPEED=1;//Absolutwert der Geschw
const float DT=(L/SPEED)/100;//Zeitschritt
const int   NSTEPS=1500;    //Anzahl Schritte
const float TDISP=0.05; //Wartezeit Grafik
//Funktionsdeklarationen:
void anfangsWerte(float & x, float & y,
		  float & u, float & v);
void bewegungsSchritt(float & x, float & y,
		      float u, float v);
void wandKollision(float x, float y,
		  float & u, float & v);
// Funktionsprototypen (Grafik -> externe Datei "grafik.cpp")
void initGrafik(  float x, float y, float R, float L);
void updateGrafik(float x, float y, float R, float L);

// Funktionsprototypen (Hilfsfunktionen - > externe Datei "util.cpp")
void initRandom();
float randomUniform();
void warten(float sekunden);

int main( ) {
  float x,y; //Positionsvektor
  float u,v; //Geschwindigkeitsvektor
  
  anfangsWerte(x,y,u,v);
  initGrafik(x,y,R,L);
  initRandom();

  for(int i=1; i <= NSTEPS; i++){
    bewegungsSchritt(x,y,u,v);
    wandKollision(x,y,u,v);
    updateGrafik(x,y,R,L);
    warten(TDISP);
  }
  
  return 0;
  }
void wandKollision(float x, float y,
		   float & u, float & v){
  if ( (x<R) || (x>(L-R)) ) {
    cout << "horiz. Kollision! " << x << endl;
    u=-u;
  }
  if ( (y<R) || (y>(L-R)) ) {
    cout << "vert.  Kollision! " << y << endl;
    v=-v;
  }
  return ; 
}
void bewegungsSchritt(float & x, float & y,
		      float   u, float   v){
  x=x+DT*u;
  y=y+DT*v;
  //cout <<" Position: " << x << " , " << y <<endl;
  return ; 
}
void anfangsWerte(float & x, float & y,
		  float & u, float & v){
  x=L/2;
  y=L/2;
  float angle=2*M_PI*randomUniform();
  u=SPEED*cos(angle);
  v=SPEED*sin(angle);
  return ; 
}
