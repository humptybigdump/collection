/*
  Bauinformatik I: Programm entwickelt in der Vorlesung V2, WS2024/25

  Umwandlung eines Temperaturwertes von Einheit Celsius in
  Einheiten Fahrenheit und Kelvin

  vermitteltes Wissen:

  * Aufbau eines C++-Programmes (main-Funktion, Bibliotheken)
  * Variablendefinition
  * arithmetische Ausdruecke
  * Zuweisung
  * Eingabe von der Tastatur
  * Ausgabe auf Konsole
 */

#include <iostream>
using namespace std; 

int main(){
  float Temp = 22.5;  //Kommentar: Definition der Variable "Temp"
  float T_F; //Temperaturwert in Fahrenheit
  float T_K; //Temperaturwert in Kelvin

  const float F_Steigung = 1.8; //konstante "Variable" 
  
  cout << "Wert von Temp = " << Temp << endl;
  
  Temp = 26;

  cout << "Wert von Temp = " << Temp << endl;

  cout << "Speicheraufwand für Temp:  " << sizeof(Temp) << endl; 
  cout << "Speicheraufwand für float: " << sizeof(float) << endl;

  cout << "Temperaturwert in Celsius eingeben:" << endl;
  cin >> Temp;
  cout << "Eingabe: " << Temp << endl;
  
  // Berechnung der Temperatur in Fahrenheit & Ausgabe:
  T_F = ( Temp * F_Steigung + 32) ;
  cout << "Temperaturwert in Fahrenheit: " << T_F << endl;

  // Berechnung der Temperatur in Kelvin & Ausgabe:
  T_K = Temp + 273.15 ;
  cout << "Temperaturwert in 'Kelvin:    "    << T_K << endl;

  return 0; //Rueckkehr (Ende des Programmes)
};
