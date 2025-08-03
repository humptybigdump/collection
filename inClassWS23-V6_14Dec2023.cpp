/*
  Partikelsimulation
  (Version ohne Grafik)

  Entworfen in der Vorlesung Bauinfo-1, V6, am 14.12.2023

  BEM: Version mit Grafik -> check 'circle_win.zip','circle_linux.tgz'

 */
#include <cmath>
#include <iostream>
using namespace std;

const float R=1;   //Radius des Partikels
const float L=10;  //Spielfeldlbreite/-höhe
const float SPEED=1;//Absolutwert der Geschw
const float DT=(L/SPEED)/10;//Zeitschritt
const int   NSTEPS=15;    //Anzahl Schritte
const float TDISP=0.1; //Wartezeit Grafik
//Funktionsdeklarationen:
void anfangsWerte(float & x, float & y,
		  float & u, float & v);
void bewegungsSchritt(float & x, float & y,
		      float u, float v);
void wandKollision(float x, float y,
		  float & u, float & v);
int main( ) {
  float x,y; //Positionsvektor
  float u,v; //Geschwindigkeitsvektor
  
  anfangsWerte(x,y,u,v);

  for(int i=1; i <= NSTEPS; i++){
    bewegungsSchritt(x,y,u,v);
    wandKollision(x,y,u,v);
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
  cout <<" Position: " << x << " , " << y <<endl;
  return ; 
}
void anfangsWerte(float & x, float & y,
		  float & u, float & v){
  x=L/2;
  y=L/2;
  u=0;
  v=SPEED;
  return ; 
}
