/* entwickelt in der Vorlesung "Bauinfo 1" (WS23/24), V3, 9.Nov.2023 */

#include <string>
#include <cmath>
#include <iostream>
using namespace std;

int main( ) {
  string op ;
  cout << "Bitte Operatorsymbol eingeben (+,-,*,/)" << endl;
  cin >> op;
  cout << "op=" << op << endl;

  cout << (op=="+") << endl;
  if(op=="+"){
    cout << "ADDITION!" << endl;
  }
  else if(op=="-"){
    cout << "SUBTRAKTION!" << endl;
  }
  else if(op=="*"){
    cout << "MULTIPLIKATION!" << endl;
  }
  else if(op=="/"){
    cout << "DIVISIOON!" << endl;
  }
  else{
    cout << "Falsches Zeichen eingegeben!" << endl;
    int i=0;
  }
  
  return 0;
  }
