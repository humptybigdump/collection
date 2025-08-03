#include <iostream>
#include <cmath>

using namespace std;

const int maxIteration = 2500;
const double deltaT = 0.01;
const double viscosity = 1e-3;
const double rhoF = 1000;
const double rhoP = 1100;

double berechneMasse(double r) {
  const double volumen = 4 * M_PI * pow(r, 3) / 3;
  return volumen * rhoP;
}

double calcDrag(double radius, double velocity) {
  return 6 * M_PI * viscosity * radius * velocity;
}

double calcGravity(double radius) {
  const double g = 9.81;
  return 4./3. * (rhoP - rhoF) * g * M_PI * pow(radius, 3);
}

double calcAcc(double mass, double force) {
  return force / mass;
}

int main() {
  double radius = 0;
  while(radius <= 0) {
    cout << "Radius eingeben: ";
    cin >> radius;
  }
/*
  do {
    cout << "Radius eingeben: ";
    cin >> radius;
  } while(radius <= 0);
*/

  const double masse = berechneMasse(radius);
    
  // Startparameter
  double velocity = 0;
  double acc = 0;
  double force = 0;

  for(int i = 0; i<maxIteration; ++i) {
    velocity += acc * deltaT;
    force = calcBuoyancy(radius) - calcDrag(radius, velocity);
    acc = calcAcc(masse, force);
    cout << "Geschwindigkeit nach " << i * deltaT << " Sekunden: " << velocity << " m/s" << endl;
  }
}