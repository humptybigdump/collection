//xparameters has the IDs of all the peripherals  also their register addresses, where it starts and where it ends
#include "xparameters.h"
//the printf function from xilinx more efficient for the targeted hardware
#include "xil_printf.h"
//contains the definitions of GPIOs and how to use them and their data types
#include "xgpio.h"
//deals with establishing xilinx custom data types
#include "xil_types.h"

//device IDs from xparameters.h
#define SW_ID XPAR_AXI_GPIO_0_DEVICE_ID
#define SUM_ID *((uint32_t *) 0x43C00000)
#define SW_CHANNEL 1
#define SW_MASK 0b1111

int main() {
	//a pointer to get the data from the GPIOs
	XGpio_Config *cfg_ptr;
	//interface to the switches
	XGpio sw_device;
	//variables for the intermmediate calculations
	u32 data,a,b,c;
	//masks to get the operands from the GPIO register
	unsigned mask1,mask2;
	mask1 = (1 << 2) - 1;
	mask2 = ((1 << 2) - 1) << 2;

	//optional just to see something running on terminal
	xil_printf("Entered function main\r\n");

	//Initialize switch Device
	cfg_ptr = XGpio_LookupConfig(SW_ID);
	XGpio_CfgInitialize(&sw_device, cfg_ptr, cfg_ptr->BaseAddress);

	//Set switch Tristate
	XGpio_SetDataDirection(&sw_device, SW_CHANNEL, SW_MASK);

	while (1) {
		//get the data from the switches
		data = XGpio_DiscreteRead(&sw_device, SW_CHANNEL);
		//we use only 4 bits but the register is 32 bits the mask ignores the other unused bits
		data &= SW_MASK;
		//get the first operand
		a = data & mask1;
		//get the second operand
		b = data & mask2;
		//scale second operand back
		b = b >> 2;
		//calculate the sum in software
		c = a + b;
		//write the sum value to the register connecting to the 7-segment controller
		SUM_ID=c;
	}
}