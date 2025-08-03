#include <iostream>
#include <math.h>
using namespace std;

/* Berechnung des Kugelvolumens
   Eingabe: r (Kugelradius)
   Ausgabe: v (Kugelvolumen)
   Author: Robin Trunk */
int main() {
  double r, v;

  cout << "Bitte Kugelradius eingben: " << endl;
  cin >> r;
  v = 4.0 / 3.0 * M_PI * r * r * r;
  cout << "Das Volumen betraegt v = " << v << endl;
  return 0;
}
