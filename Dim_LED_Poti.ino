int Poti;   //create variable Poti

void setup() {
Serial.begin(9600);
Poti = 0;   // assign the value 0 to Poti
pinMode(11, OUTPUT);    // define pin 11 as OUTPUT
}

void loop() {
Poti = analogRead(A0);    // value of pin A0 is assigned to Poti
Serial.print("Wert des Potis:");    // print "Wert des Potis" in serial monitor
Serial.print(Poti);   // print value of Poti in serial monitor
Serial.print("");   // print new line in serial monitor
analogWrite(11,map(Poti,0,1023,0,255));   // write value of Poti on pin 6 , 
//transpose values between 0 and 1023 from analog pin to values between 0 and 255
}
