# STM32Fxxx micro-controller hacking in Haskell

## STM32-Zombie

The STM32-Zombie library turns a STM32 micro-controller
into a powerful Haskell-hackable hardware interface.
It gives the user full control of the STM32Fxxx hardware peripherals
without the need to write any c-code, without cross-compiler tool chain
and even without any particular micro controller firmware.
The library is called STM-32 Zombie because it halts the brain,
i.e. the ARM CPU of the controller board and instead uses the on-cip-debugging
features of the controller. The boards run like a Zombie without using
its own brain.

## Compatible Hardware
I have tested the library with the following hardware:

* ST-Link USB dongle (Google for "ST-Link V2 stlink mini STM8STM32 STLINK simulator")

![ST-Link USB dongle](https://i.imgur.com/Y4iEvVt.jpg)

* A STM32F103 breakout board (Google for " "STM32F103C8T6 ARM STM32 Minimum System Development Board Module")

![STM32F103C8T6 board](https://imgur.com/Xb4mOFa.jpg)
![STM32F103C8T6 board](https://imgur.com/qMCp6au.jpg)

The USB dongle as well as the breakout boards are available in China starting
at about US$2.
The cheap ST-link clones and the breakout boards work well but the US$2
ST-Link clones provide no electrical isolation to protect your PC or Laptop.
Experimenting with Hardware is a lot of fun until you let the magic smoke out.
Brand ST-Link boards, that guarantee electrical isolation and full protection
of the PC, are very affordable and may be a good investing.

## How it Works
One end of ST-Link dongle plugs into the PC USB port the other end is the so called
SWD (single wire debug) interface.
The SWD interface, which also provides electrical power, is connected to the
STM32F103 breakout with 4 jumper wires.
As most breakout boards come with a LED, this is all that is needed to run the
App.Blink example.
Via the SWD interface the PC can read and write the controller CPU
address space and access the memory-mapped hardware registers.

## API
The STM32-Zombie library is modeled after the STMicroelectronics 
STM32F10x firmware library which provides a low level interface to the 
controller hardware.
This API is suitable for bare metal hardware hacking but also allows to build
higher level interfaces.

## Examples and features
The STM32Fxxx controllers feature a wide variety of powerful and flexible
hardware peripherals like GPIO port, serial, SPI, I2C interfaces
and 12bit-ADC converters and USB ports.
The killer feature is, that they also include a flexible DMA controller
that can be uses in combination with the peripherals.
This makes it possible to build hard-real-time applications that work
completely independent from the controller CPU.
Examples are ADC sampling and high sampling rate and with precise timing,
high frequency sampling of digital inputs or generation of high frequency
digital output patterns.

The library does not cover all of the STM32 hardware.
I add support for something when I need it in a particular hardware project.
(For example I have not tried the USB features of the boards,
which seems to be very interesting project)

The examples are in the App module hierarchy.

### App.Blink
Blink a LED.
### App.ADC
Buffered ADC converters with DMA transfer.
### App.DMABuffer
An example for a serial ports with DMA transfer.
### App.TestLCD
An LCD driver.
This is the original code from the hArduino library
with some very small adaptions.
### App.WS1228B
A driver for nice colorful RGB LED strips.
(Uses SPI and DMA) 
### App.Serial
Hello world example for serial ports.

## Todo

## Support packages
Parts of the functionality are packaged separately

### STLinkUSB
The STLinkUSB package contains a Haskell driver for ST-Link USB dongles.
The library is based on information from the openocd library.
The STM32-Zombie package only uses a small subset of the ST-Link features
and only these features of ST-Link protocol are really tested and included
in STLinkUSB. (There is some extra but untested code). 
[Haddock documentation](http://hackage.haskell.org/package/STLinkUSB)


### STM32F103xx-SVD
This package contains names and definitions for STM32F103 peripherals,
registers, addresses and the bits of the hardware registers.
It is generated from STM32F103xx.svd.
[Haddock documentation](http://hackage.haskell.org/package/STM32F103xx-SVD)

### SVD2HS
The compiler that translates a file called STM32F103xx.svd to a set of
Haskell data types and lookup tables.