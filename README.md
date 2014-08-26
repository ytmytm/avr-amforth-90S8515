amforth for AVR AT90S8515 with external RAM support
====================================================

This is a form of [amforth 1.6 (or 1.7)](http://amforth.sourceforge.net/) from Spring of 2007
that cuts down this already minimal Forth to support 8515 device.

Support for compiling words into external RAM instead of flash is provided
through words replacements in nwords/ directory.

The assembler syntax is compatible with avrasm32 assembler 1.74 from ATMEL (maybe also by later versions).

This was never tested on a real device, only in simulator [VMLAB](http://www.amctools.com/vmlab.htm).

Maciej Witkowiak, 2014-08-27
