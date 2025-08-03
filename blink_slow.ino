// Simple LED blink

const int led = LED_BUILTIN;

void setup() {

    Serial.begin(9600);
    Serial.println("Boot");
    
  pinMode(led, OUTPUT);
  Serial.println("Boot end");
}

void loop() {
  Serial.println("loop");
  digitalWrite(led, HIGH);
  delay(1000);
  digitalWrite(led, LOW);
  delay(1000);
}
