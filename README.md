amforth for AVR AT90S8515 with external RAM support
====================================================

This is a form of [amforth 1.6](http://amforth.sourceforge.net/) from Spring of 2007
that cuts down this already minimal Forth to support 8515 device.

Support for compiling words into external RAM instead of flash is provided
through words replacements in nwords/ directory.

The assembler syntax is compatible with avrasm32 assembler 1.74 from ATMEL (maybe also by later versions).

This was never tested on a real device, only in simulator [VMLAB](http://www.amctools.com/vmlab.htm).

Maciej Witkowiak, 2014-08-27

# Build
Just call
```
avrasm32 forth.asm
```

Some words are included directly, some are merge into words_low.asm file by a script join.sh, which just
takes the list of includes from dict_low and concatenates files.

# Simulate
Simulator for Windows is in simulator folder. Open forth.prj through projects. For speedup turn off code animation.
