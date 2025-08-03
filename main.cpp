/*
 * main.cpp
 *
 *
 */

#include "Timer.h"
#include "GPIO.h"


Timer timer1;
GPIO led;
System system;


void ISR()
{
    timer1.clearInterruptFlag();

    led.write(!led.read());
}



int main(void)
{
    system.init(40000000);

    led.init(&system, GPIO_PORTF_BASE, GPIO_PIN_1, GPIO_DIR_MODE_OUT);

    timer1.init(&system, TIMER1_BASE, ISR, 5);

    timer1.start();

    // The whole code runs in the ISRs, hence an empty while loop.
    while (1)
    {

    }
}
