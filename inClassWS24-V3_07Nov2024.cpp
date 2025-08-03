/* Demoprogramm "Taschenrechner in C++"
   
   entwickelt in der Vorlesung "Bauinfo 1" (WS24/25), V3, 7.Nov.2024
*/

#include <cmath>
// Bibliothek 'string' erlaubt (komfortable) Verarbeitung von Zeichenketten
#include <string>
#include <iostream>
using namespace std;

const string add="+";
const string sub="-";
const string mult="*";
const string sdiv="/";

int main( ) {
  cout << boolalpha; //schaltet Ausgabe von 'bool' auf true/false
  
  string eingabe;
  double a,b,r=0.0;
  cout << "Bitte eine Zahl eingeben (Operand #1):" << endl;
  cin >> a;
  cout << "Bitte ein Zeichen eingeben (+,-,*,/):" << endl;
  cin >> eingabe;
  cout << "Bitte eine Zahl eingeben (Operand #2):" << endl;
  cin >> b;
  cout << "Eingabe: " << a << " " << eingabe << " " << b << endl; 
  // Vergleich der Eingabe mit einem Symbol:
  cout << "Vergleich: " << (eingabe == "+") << endl;
  //if (eingabe == "+"){ //Vergleich mit literaler Konstante
  if (eingabe == add){ //Vergleich mit konstanter Variable
    cout << "Addition!" << endl;//wird nur erreicht, wenn
                                //Bedingung 'wahr'
    r=a+b;
  }
  else if (eingabe == sub){
    cout << "Subtraktion!" << endl;
    r=a-b;
  }
  else if (eingabe == mult){
    cout << "Multiplikation!" << endl;
    r=a*b;
  }
  else if (eingabe == sdiv){
    cout << "Division!" << endl;
    r=a/b;
  }
  else{
    cout << "(keines der definierten Operatorzeichen)" << endl;
    return 1; 
  }
  cout << "Resultat: " << a << eingabe << b << "=" << r << endl;
  return 0;
}
