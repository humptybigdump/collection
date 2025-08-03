#include <iostream>
using namespace std;


// Void Unterprogramm -> ohne return
void sage (string text){
	cout << "Ich sage " << text << endl;
}

//Beginn des Hauptprogrammes:
int main (){
	sage ("Hallo");
	sage ("Ich lerne C++");
	
	return 0;
}
