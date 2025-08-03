#include <iostream>
#include <iomanip>
using namespace std;


// Funktion, die die Matrix A in L und R zerlegt:
void LRZerl(double** A, double** L, double**R,  int n) {
    for (int i = 0; i < n; i++) {
        R[i][i] = 1.0;
        for (int j = 0; j <n; j++) {
            if (j <= i) {
                double sum = 0;
                for (int k = 0; k <= j; k++) {
                    sum += L[i][k] * R[k][j];
                }
                L[i][j] = A[i][j] - sum;
            }
            else if (j >= i + 1) {
                double sum = 0;
                for (int k = 0; k <= j; k++) {
                    sum += L[i][k] * R[k][j];
                }
                R[i][j] = (A[i][j] - sum) / L[i][i];
                for (int k=0; k<n; k++) {
                    R [k][k]=1.0;
            }
            
        }
    }
}
}
// Funktion, die das lineare Gleichungssystem loest
void Solve(double** L, double** R, double* b, double* x, double* y, int n) {
    
    for (int i = 0; i < n; i++) {
        y[i] = b[i];
        for (int k = 0; k <= (i - 1); k++) {
            y[i] -= (L[i][k] * y[k]);
        }
        y[i] /= L[i][i];
    }
    for (int i = n-1; i >= 0; i--) {
        x[i] = y[i];
        for (int k = i+1; k <n; k++) {
            x[i]-= R[i][k] * x[k];
        }
    }
}

// Funktion Eingabe:
void Eingabe(int n, double**A, double*b) {
    // A & b einlesen:
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cout << "Matrix A Eintrag fuer Zeile " << i + 1 << "Spalte " << j + 1 << endl;
            cin >> A[i][j];
        }
    }
    for (int i = 0; i < n; i++) {
    cout << "Bitte b eingeben fuer die Zeile" << i + 1 << endl;
    cin >> b[i];
    }
}


// Funktion Ausgabe:
void Ausgabe (double* b, int n) {
    for (int i=0; i<n; i++){
    cout << b[i] << endl;
    }
}


int main() {
    int n;
    cout << "Bitte n eingeben";
    cin >> n;
    // dynamischer Speicher y & b & x:
    double* y = new double[n];
    double* b = new double[n];
    double* x = new double[n];
    
    // dynamischer Speicher A:
    double** A = new double* [n];
    for (int h = 0; h < n; h++) {
        A[h] = new double[n];
    }

    // dynamischer Speicher L&R:
    double** L = new double* [n];
    double** R = new double* [n];
    for (int h = 0; h < n; h++) {
        L[h] = new double[n];
        R[h] = new double[n];
    }

    Eingabe( n, A, b);
    LRZerl(A, L, R, n);
    Solve (L, R, b, x, y, n);
    Ausgabe (x,n);

    // Speicher wieder freigeben
    for (int i = 0; i < n; i++) {
        delete[] L[i];
        delete[] R[i];
        delete[] A[i];
    }
    delete []A;
    delete []L;
    delete []R;
    
    delete []b;
    delete []x;
    delete []y;


    return 0;
}


