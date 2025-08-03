/* Demoprogramm "Bauinfo 2", V7, 25.6.2024
   
   Klassen - Konstruktor/Destruktor 
   & dynamische Speicherverwaltung

   HIER: Ausnahmebehandlung bei Speicheranforderung

   (aus Vorlage "selbsttest_dyn_var_exception-VORLAGE.cpp")

   To-be-done:
   * Abfangen von positiven, zu grossen Speicherlaengen
      ('new nothrow float [N]' und Test fuer Nullpointer)

 */

//"stdlib" enthaelt u.a. Funktion "exit"
#include <stdlib.h> 

#include <cmath>
#include <iostream>
using namespace std;

const int N1=100;
const int N2=200;
//Klasse, welche als 'Signal' beim Auftreten einer Ausnahme dient
class Speicher{
public:
  void show(){
    cout << "Speicherproblem!" << endl;
  }
};
// eigentliche Vektorklasse mit dyn. Speicher
class myVec{
  float * data;
  int length;
public:
  myVec();    //Standardkonstruktur
  myVec(int n);
  ~myVec();   //Standarddestruktor

  void setData(int n,float value);
};

int main( ) {
  int nvec;
  bool fehler = true;
  while(fehler){
    cout << "Laenge des Vektors eingeben: " << endl;
    cin >> nvec;
    try{//kritischer Code: Speicheranforderung (in Konstruktor)
      myVec vec4(nvec);
      //alles gut gegangen
      fehler = false; 
    }
    catch(Speicher & v){
      cout << "im catch-Block gelandet! " << endl;
      v.show();
    }
  }

  return 0;
  }

myVec::myVec(){
  //Standardlaenge = 1
  length = 1;
  data = new float[length];
  cout << "myVec Standard-Konstruktor: n=1 " << &data << endl;
}

myVec::myVec(int n){
  //Laenge wird aus Parameter uebernommen
  if (n>0){
    length = n;
    data = new float[length];
    cout << "myVec Konstruktor: n=" << n << " " << &data << endl;
  }
  else{//BEM: hier fangen wir lediglich negative Speicherlaengen ab
    cout << "myVec Konstruktor: n=" << n << " illegal!" << endl;
    //exit(EXIT_FAILURE); //beendet Programmausfuehrung
    Speicher vec;
    throw vec;
  }
}

myVec::~myVec(){
  cout << "Destruktor. " << &data << endl;
  delete [] data;
}

void myVec::setData(int n, float value){
  if (n>=0 & n<length)
    data[n]=value;
  return ;
}
