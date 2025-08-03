// Definition der Variablen potPin und ledPin
int potPin = 0;  // potPin: Messwert Spannnung an Poti
int ledPin = 11; // ledPin: digitaler Pin von Diode
int val = 0; // Platzhalter für Spannung an Poti

void setup() {
pinMode(ledPin, OUTPUT);  // ledPin als OUTPUT deklarieren
}

void loop() {
val = analogRead(potPin);   // Einlesen des Messwerts Spannnung
                            // Achtung: Der ADC arbeitet mit 10 Bit
                            // Auflösung, d.h. 0 < potPin < 1023
digitalWrite(ledPin, HIGH); // Anschalten der LED
delay(val);                 // Stoppen des Programms für 0-1023 ms
                            // d.h. LED bleibt für 0-1023 ms an
digitalWrite(ledPin, LOW);  // Ausschalten der LED
delay(val);                 // Stoppen des Programms für 0-1023 ms
                            // d.h. LED bleibt für 0-1023 ms aus
}
