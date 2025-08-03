
// the setup function runs once when you press reset or power the board
// define pin 12 for LED1 and pin 11 for LED2
int LED1 = 12;
int LED2 = 11;
void setup() {
  // initialize digital pins 12 and 11 as an output.
  pinMode(LED1, OUTPUT);
  pinMode(LED2, OUTPUT);
}

// the loop function runs over and over again forever
void loop() {
  digitalWrite(LED1, HIGH);   // turn the LED1 on (HIGH is the voltage level)
  digitalWrite(LED2, LOW);    // turn the LED2 off by making the voltage LOW
  delay(200);                // wait for a second
  digitalWrite(LED1, LOW);    // turn the LED1 off by making the voltage LOW
  digitalWrite(LED2, HIGH);   // turn the LED2 on (HIGH is the voltage level)
  delay(200);                // wait for a second
}
