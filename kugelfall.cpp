#include <iostream>
#include <cmath>
using namespace std;

int main() {
  double radius = 0.025;
  double particleDensity = 1100;
  double fluidDensity = 1000;
  double viscosity = 1e-3;

  double velocity = 0;
  double acc = 0;
  double force = 0;
  double deltaT = 0.001;
  double mass = 4 * M_PI * pow(radius,3) / 3;
  for(int i=0; i<5000; ++i) {
    double gravitation = 4./3. * (particleDensity - fluidDensity) * 9.81 * M_PI * pow(radius,3);
    double drag = 6 * M_PI * viscosity * radius * velocity;
    force = gravitation - drag;
    acc = force/mass;
    velocity += acc * deltaT;
  }
  cout << "iterative result: " << velocity << " m/s" << endl;

  double analyticalResult = 
    (2./9.) 
    * (particleDensity - fluidDensity)
    * 9.81 * pow(radius, 2) / viscosity;
    
    cout << "analytical result: " << analyticalResult << " m/s" << endl;
}