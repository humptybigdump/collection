/* Demoprogramm "Bauinfo 2", V6, 18.6.2024
   *** weiterentwickelt in V7, 25.6.2024 ***
   
   Klassen - Konstruktor/Destruktor,
   dynamische Speicherverwaltung
   & überladene Operatoren

   * "Regel der drei" (Destruktor, copy-Konstruktor &
      Zuweisungsoperator)
   * 'shallow-copy' vs. 'deep-copy'
   * Beispiel: Vektorklasse
        (aus Vorlage "vorlage_vec_class.cpp")
   * Operator '+'
   
 */

//"stdlib" enthaelt u.a. Funktion "exit"
#include <stdlib.h> 

#include <cmath>
#include <iostream>
using namespace std;

const int N=3;  //Standardlaenge

class myVec{
  //float data[N];      //<-statisch definiert
  float * data = NULL;  //<-dynamisch definiert
  int length;
public:
  myVec();    //Standardkonstruktur
  myVec(int );    //Konstruktor #2
  myVec(const myVec &);    //copy-Konstruktur
  ~myVec();   //Standarddestruktor
  myVec & operator=(const myVec &); //Zuweisungsoperator
  myVec  operator+(const myVec &) const; //Addition
  void setData(int n,float value);
  void show() const;
};

int main( ) {
  myVec vec1;
  myVec vec2=vec1;  //<-ruft copy-Konstruktor auf
  myVec vec3(5);
  vec2=vec1;        //<-Zuweisungsoperator
 
  vec1.show();
  vec2.setData(1,-4.3);
  vec2.show();

  myVec vec4;
  vec4=vec1+vec2;  // vec4=vec1.operator+(vec2);
  vec4.show();
  
  vec2=vec1;        //<-Zuweisungsoperator
  vec2.show();

  return 0;
  }
myVec myVec::operator+(const myVec & v) const{
  myVec r;
  for (int i=0; i<length; i++){
    r.data[i]=data[i]+v.data[i];
  }
  return r;
}
myVec & myVec::operator=(const myVec & v){
  //Zuweisungsoperator
 for (int i=0; i<length; i++){
    data[i]=v.data[i];
  }
  cout << "myVec Zuweisungsoperator: n=" << length << " " <<
    data << endl;
  return *this ;
}; 
myVec::myVec(){
  length = N;
  data = new float [N];
  cout << "myVec Standard-Konstruktor: n=" << length << " " <<
    data << endl;
}
myVec::myVec(const myVec & v){//copy-Konstruktor
  length = N;
  data = new float [N];
  for (int i=0; i<length; i++){
    data[i]=v.data[i];
  }
  cout << "myVec copy-Konstruktor: n=" << length << " " <<
    data << endl;
}
myVec::myVec(int k){
  length = k;
  data = new float [k];
  for (int i=0; i<length; i++){
    data[i]=0.0;
  }
  cout << "myVec Konstruktor #2: n=" << length << " " <<
    data << endl;
}

myVec::~myVec(){
  delete [] data; 
  cout << "Destruktor. " << data << endl;
}

void myVec::setData(int n, float value){
  if (n>=0 & n<length)
    data[n]=value;
  return ;
}
void myVec::show() const{
  for(int i=0; i<length;i++){
    cout << i << ": " << data[i] << endl;
  }
  return ;
}
