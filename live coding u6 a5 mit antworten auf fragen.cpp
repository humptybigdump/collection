#include <iostream>
using namespace std;

class Apparat {
private:
	// Variable die nur in der Klasse zugänglich ist (siehe Aufgabenstellung)
	double shift;
	
public:
	// Konstruktor (siehe Aufgabenstellung)
	Apparat (double rho) {
		shift = 0.5 * rho / 2300;
	}
	
	// Copy-Constructor (hier zum erstellen einer Klasse aus der selben Klasse)
	Apparat (const Apparat &old) {
		this->shift = old.shift;
	}
	
	// Änderung von shift (siehe Aufgabenstellung)
	void changeRho (double rho) {
		shift = 0.5 * rho / 2300;
	}
	
	// Berechnung der Trennschärfe (siehe Aufgabenstellung)
	void calculateT(double diameter) {
		double T = 0.;
		
		if(diameter < 2+shift) {
			T = 0.;
		}
		else if (diameter > 7 + shift) {
			T = 1.;
		}
		else {
			T = 0.2 * (diameter - shift) - 0.4;
		}
		
		cout << "Bei einem Durchmesser von " << diameter << " werden " << T*100 
		     << "% der Partikel abgeschieden." << endl;
	} 
	
// Kann man private: ... public: ... private: ... abwechselnd schreiben? Siehe den Test:
// Geht, sollte man aber nicht machen, da es extrem unübersichtlich wird!
private:
	double t;
	
public: 
// Muss man das 'this->' vor die Variable schreiben?
// Wird nur benötigt, wenn eine andere Variable im Gültigkeitsbereich den selben Namen trägt.
// Wie in diese beiden Beispiele:
	void test(double shift) {
		cout << "this->shift:\t" << this->shift << endl;
		cout << "shift:\t\t" << shift << endl;
	}
	
	void test2() {
		double shift = 200;
		cout << "this->shift:\t" << this->shift << endl;
		cout << "shift:\t\t" << shift << endl;
	}
};

int main() {
	// Erstellen eines Objekts mit dem Konstruktor
	Apparat test (2300.);
	test.calculateT(6);
	
	// Erstellen eines Objekts mit dem Copy-Konstruktor
	Apparat test2 (test);
	test2.calculateT(6);
	
	// Test für die Frage nach 'this->' (s. o.) 
	test2.test(500);
	test2.test2();
	
	// Wie erstellt man einen Pointer auf ein Objekt einer Klasse?
	Apparat* app = new Apparat(2300);
	// Kann man ein Array aus Klassenobjekten erstellen?
	Apparat* appF[3]; 
	appF[0] = new Apparat(1240);
	appF[1] = new Apparat(2300);
	appF[2] = new Apparat(test);
	// ACHTUNG: Dann wird die Memberfunction allerdings anders aufgerufen, da es sich nun um Pointer auf das Objekt handelt. (s.u.)
	appF[0]->calculateT(4);
	appF[1]->calculateT(2);
	appF[2]->calculateT(6);
	
	return 0;
}