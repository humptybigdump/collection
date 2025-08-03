void setup() {
  // put your setup code here, to run once:
 Serial.begin(9600);			// set the "printf" debug to 9600 baud -> set your terminal to 9600 baud!
 Serial.println("Boot starts");
 pinMode(13, OUTPUT);
}

void loop() {
  // put your main code here, to run repeatedly:
  Serial.println("loop");
  int AnalogSensor = analogRead(A0);
  Serial.println(AnalogSensor);

  digitalWrite(13, HIGH);
  delay(AnalogSensor);
  digitalWrite(13,LOW);
  delay(AnalogSensor);

}
