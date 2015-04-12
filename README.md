# fp-avrsim
AVR simulator ported to FreePascal, with integrated GDB server.

## Compiling
**lazbuild avrsim.lpi**

or

**fpc avrsim.lpr**

## Running
*avrsim file.bin*

Normal simulation



*avrsim -d2159 file.bin*

Starts a new simulation, halts at address 0, and waits for GDB Remote connections on the given port(2159 in this example).
