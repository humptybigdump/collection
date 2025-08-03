#include <iostream>
using namespace std;

int main() {

double m, Mneu, Malt, Maltalt, h, k, delta, mult;


delta = 1;
while ((1 + delta) != 1) {
  delta = delta / 2;
}

h = 1;
while (h != 0) {
  m = h; h = h / 2;
}

mult = 1.001;
Malt = 0;
Mneu = 1;
while (Mneu != Malt) {
  Maltalt = Malt; Malt = Mneu; Mneu *= mult;
}

/* Irgendwann sind Mneu=inf und Malt=inf;
Malt ist dann der letzte erhaltene Wert
Genauigkeit erhoehen: mult naeher an 1 waehlen;
*/
cout << "Maschinengenauigkeit=" << 2 * delta << endl;
cout << "Kleinste positive Zahl=" << m << endl;
cout << "Groesste Maschinenzahl=" << Maltalt << endl;
return 0;
}
