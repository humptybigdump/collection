/*
 * Timer.cpp
 *
 *    Author: Max Zuidberg
 *     Email: m.zuidberg@icloud.com
 */


// Uncomment following #define to use the precompiled Timer library instead of
// the code in this file.
// #define USE_TIMER_LIBRARY

#ifndef USE_TIMER_LIBRARY


#include <Timer.h>


Timer::Timer()
{
    /*
     * Default empty constructor
     */
}

Timer::~Timer()
{
    /*
     * Default empty destructor
     */
}

void Timer::init(System* sys, uint32_t base, void (*ISR)(void), uint32_t freq)
{
    this->sys = sys;
    this->base = base;
    uint32_t i_wanna_see_freq = freq;

    switch (base)
    {
    case TIMER0_BASE:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER0);
        break;
    case TIMER1_BASE:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER1);
        break;
    case TIMER2_BASE:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER2);
        break;
    case TIMER3_BASE:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER3);
        break;
    case TIMER4_BASE:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER4);
        break;
    case TIMER5_BASE:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER5);
        break;
    }

    sys->delayCycles(5);

    TimerConfigure(base, TIMER_CFG_PERIODIC | TIMER_CFG_A_ACT_TOINTD);

    TimerIntEnable(base, TIMER_TIMA_TIMEOUT);

    TimerIntRegister(base, TIMER_A, ISR);

    setFreq(freq);
}

void Timer::clearInterruptFlag()
{
    TimerIntClear(base, TIMER_TIMA_TIMEOUT);
}

void Timer::setFreq(uint32_t frequency)
{
    this->freq = frequency;

    if (frequency == 0)
    {
        stop();
    }
    else
    {
        uint32_t load = sys->getClockFreq() / frequency;

        TimerLoadSet(base, TIMER_A, load);
    }
}

void Timer::setPeriodUS(uint32_t periodUS)
{
    this->periodUS = periodUS;

    if (periodUS == 0)
    {
        stop();
    }
    else
    {
        uint32_t load = sys->getClockFreq() / 1000000 * periodUS;

        TimerLoadSet(base, TIMER_A, load);
    }
}

void Timer::start()
{
    TimerEnable(base, TIMER_A);
}

void Timer::stop()
{
    TimerDisable(base, TIMER_A);
    clearInterruptFlag();
}

uint32_t Timer::getFreq()
{
    // TODO freq <-> period
    return freq;
}

uint32_t Timer::getPeriodUS()
{
    // TODO freq <-> period
    return periodUS;
}


#endif
