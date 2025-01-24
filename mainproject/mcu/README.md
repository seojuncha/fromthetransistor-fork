# From the Transistor Microcontroller

## Initial Process
1. Start or reset
2. MCU runs BootROM code stored in flash
3. BootROM checks flash memory for a valid bootloader.
4. If valid, jump to bootloader code.
   1. Bootloader connets to UDP server, an host PC, that have a kernel image.
   2. Download the kernal image ...(?)
5. If not valid, try to download bootloader via serial.
   1. Initialize serial communication.
   2. Wait to read data
   3. If done, check validation of bootloader
   4. If passed, jump to bootloader code.
6. 


## BootROM