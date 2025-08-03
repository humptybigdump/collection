#include <iostream>
#include <cmath>
using namespace std;

double calcBuoyancy(double rhoP, double rhoF, double radius)
{
    const double g = -9.81;
    return 4./3. * (rhoP-0 * rhoF) * g * M_PI * pow(radius,3);
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
    const double deltaT = 0.01;
    int n = 0;

    // Startparameter
    double vel[3] = {3, 0, 0};
    double acc[3] = {0, 0, 0};
    double pos[3] = {0, 100, 0};
    double force[3];
    double buoyancy[3] = {0, calcBuoyancy(rhoP, rhoF, radius), 0};

    while(pos[1]>0) {
        for(int i=0; i<3; ++i) {
            vel[i] += acc[i] * deltaT;
            force[i] = buoyancy[i] - 0 * calcDrag(viscosity, radius, vel[i]);
            acc[i] = force[i] / mass;
            pos[i] += vel[i] * deltaT;
        }
        cout << "Geschwindigkeit nach " << n * deltaT
             << " Sekunden:" << endl
             << " Geschwindigkeit: (" << vel[0] << ", " << vel[1] << ", " << vel[2]
             << ") in m/s" << endl
             << " Position: (" << pos[0] << ", " << pos[1] << ", " << pos[2]
             << ") in m" << endl;
        ++n;
    }

    return 0;
}
