#!/usr/bin/python
# coding=utf-8

# import packages
import RPi.GPIO as GPIO
import Adafruit_DHT
import time

# set the pause between measures
sleeptime = 2

# Sensor should be set to Adafruit_DHT.DHT11,
# Adafruit_DHT.DHT22, or Adafruit_DHT.AM2302.
DHTSensor = Adafruit_DHT.DHT11

# set PIN mode (GPIO-numbers with GPIO.BCM or board pin numbers with GPIO.BOARD)
GPIO.setmode(GPIO.BCM)

# declare gpio pins
temp_pin = 23
red_pin = 4
green_pin = 17
blue_pin = 18

# for the LED set the pins to output mode
GPIO.setup(red_pin, GPIO.OUT, initial=GPIO.LOW) 
GPIO.setup(green_pin, GPIO.OUT, initial=GPIO.LOW)
GPIO.setup(blue_pin, GPIO.OUT, initial=GPIO.LOW)


# define function to set the color
def set_color(red_int, green_int, blue_int):
    ###
    # red, green, blue: boolean
    ###
    if (red_int):
        GPIO.output(red_pin, GPIO.HIGH)
        GPIO.output(green_pin, GPIO.LOW)
        GPIO.output(blue_pin, GPIO.LOW)
    if (green_int):
        GPIO.output(red_pin, GPIO.LOW)
        GPIO.output(green_pin, GPIO.HIGH)
        GPIO.output(blue_pin, GPIO.LOW)
    if (blue_int):
        GPIO.output(red_pin, GPIO.LOW)
        GPIO.output(green_pin, GPIO.LOW)
        GPIO.output(blue_pin, GPIO.HIGH)
    else:
        GPIO.output(red_pin, GPIO.LOW)
        GPIO.output(green_pin, GPIO.LOW)
        GPIO.output(blue_pin, GPIO.LOW)
    return


print('KY-015 sensor and KY-016 LED test')

try:
    while(1):
        # read from the sensor
        humidity, temperature = Adafruit_DHT.read_retry(DHTSensor, temp_pin)

        print("-----------------------------------------------------------------")
        if humidity is not None and temperature is not None:
            # print measures
            print('Temperatur = {0:0.1f}°C  | rel. Luftfeuchtigkeit = {1:0.1f}%'.format(temperature, humidity))

            if (temperature >= 25):
                # fever => red
                set_color(1, 0, 0)

            elif (temperature <= 23):
                # cold => blue
                set_color(0, 0, 1)

            else:
                # normal => green
                set_color(0, 1, 0)

        else:
            # some timing error occurred
            print('Fehler beim Auslesen - Bitte warte auf den nächsten Versuch!')
        print("-----------------------------------------------------------------")
        print("")
        time.sleep(sleeptime)

# clean up the GPIO stuff after interrupt through ctrl+c
except KeyboardInterrupt:
    GPIO.cleanup()
