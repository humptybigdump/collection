//Untersuchung der Effizienz von Sortieralgorithmen:
//Merge Sort (rekursiver Sortieralgorithmus -> durchschnittlicher Rechenaufwand O(n log(n))

#include <iostream>
#include <time.h>
#include <cmath>
using namespace std;

void merge(double* feld, int l, int m, int r) {
    int i, j, k, nl, nr;
    //Groesse der Teilfelder:
    nl = m - l + 1;
    nr = r - m;
    double lfeld[nl], rfeld[nr];

    //Teilfelder mit Feld befuellen:
    for (i = 0; i < nl; i++) {
        lfeld[i] = feld[l + i];
    }
    for (j = 0; j < nr; j++) {
        rfeld[j] = feld[m + 1 + j];
    }

    i = 0; j = 0; k = l;
    //Teilfelder in richtiges Feld sortieren
    while (i < nl && j < nr) {
        if (lfeld[i] <= rfeld[j]) {
            feld[k] = lfeld[i];
            i++;
        }
        else {
            feld[k] = rfeld[j];
            j++;
        }
        k++;
    }
    while (i < nl) {
        feld[k] = lfeld[i];
        i++; k++;
    }
    while (j < nr) {
        feld[k] = rfeld[j];
        j++; k++;
    }
}

void mergeSort(double* feld, int l, int r) {
    int m;
    if (l < r) {							//linke (untere) und rechte (obere) Grenze des Feldes
        int m = l + (r - l) / 2;				//Ermittlung der Mitte des Feldes um dann Teilfelder zu sortieren
        mergeSort(feld, l, m);			//Rekursiver Aufruf der Funktion
        mergeSort(feld, m + 1, r);
        merge(feld, l, m, r);
    }
}


int main() {

    //Definition des zu sortierenden Feldes:
    unsigned int alength = 10000;
    double* feld = new double[alength];		//dynamisches Feld (siehe Kap. 14)

    for (int i = 0; i < alength; i++) {
        feld[i] = 5000 - i;
    }

    //Rechenzeit mesen:
    double time = 0.0, tstart;		//Zeit Vaiablen
    tstart = clock();				//Beginn der Messung

    //Aufruf des Algorithmus:
    mergeSort(feld, 0, alength - 1);
    cout << "das Feld ist sortiert: " << feld[0] << " , " << feld[alength - 1] << endl;

    //Beenden der Zeitmessung:
    time += clock() - tstart; 		//Ende der Messung
    time = time / CLOCKS_PER_SEC;  // Umscalierung in Sekunden
    cout << "Zeit = " << time << " sec." << endl;

    return 0;
}
