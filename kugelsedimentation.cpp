#include <iostream>
#include <cmath>
using namespace std;

double calcBuoyancy(double rhoP, double rhoF, double radius)
{
    const double g = 9.81;
    return 4./3. * (rhoP-rhoF) * g * M_PI * pow(radius,3);
}

double calcDrag(double viscosity, double radius, double vel)
{
    return 6 * M_PI * viscosity * radius * vel;
}

int main()
{
    // Stoffparameter
    const double viscosity = 1e-3;
    const double rhoF = 1000;
    const double rhoP = 1100;

    // Geometrie
    double radius;
    cout << "Bitte geben Sie den Radius an: " << endl;
    cin >> radius;

    const double mass = 4./3. * M_PI * pow(radius, 3) * rhoP;


    // Iterationsparameter
    const int it = 5000;
    const double deltaT = 0.1;

    // Startparameter
    double vel = 0;
    double acc = 0;
    double force;

    for(int i = 0; i<it; ++i) {
        vel += acc * deltaT;
        force = calcBuoyancy(rhoP, rhoF, radius)
                - calcDrag(viscosity, radius, vel);
        acc = force / mass;
        cout << "Geschwindigkeit nach " << i * deltaT
             << " Sekunden: " << vel
             << " m/s" << endl;
    }

    return 0;
}
