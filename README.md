# S10FPGA

## Overview

Library containing Bluespec and Verilog components targetting Stratix 10 FPGAs.


## Low-level library components

### ChipID

A simple Bluespec wrapper around the chipid.ip component to make it
easy to read the unique ChipID on Stratix 10 FPGAs.

  * `chipid.ip` - IP definition file
  * `ChipID.bsv` - BSV wrapper around the chipid library


### S10Synchronizer

Wrapper around Intel's standard clock-crossing synchronizer.  There
are four DFFs (which is a bit excessive!): the first latches data in
the source clock domain, the next three are inside the standard
synchronizer to perform the clock crossing.  Note that Quartus
specific directives inside the `altera_std_synchronizer` library
component ensure timing closure is met.

  * `S10Synchronizer.bsv` - BSV interface
  * `s10syncronizer.v` - Verilog wrapper around the `altera_std_synchronizer` library component


### S10FIFO

Wrapper around Intel's dual-clock FIFO to ensure timing closure when
transitioning more than one bit-width of data between clock domains.

  * `s10dcfifo.v` - Verilog instantiation of the dcfifo component,
    parameterised on data width and FIFO depth
  * `s10dcfifo.sdc` - timing constraints for the above
  * `S10FIFO.bsv` - BSV wrapper around the dcfifo instantiation
  

# Higher-level components

## BERT

A bit error-rate tester for SerialLite III.  The built design is meant
to be instantiated inside Qsys.  It provides a 32-bit wide AXI4Lite
interface to internal control/status registers to allow serial links
to be tested.  See the NIOS II code
[main.c](https://github.com/CTSRD-CHERI/de10pro-seriallite3/blob/main/software/app/main.c)
in the
[de10pro-seriallite3](https://github.com/CTSRD-CHERI/de10pro-seriallite3)
repository provides examples of use.

 * `BERT.bsv` - the BSV implementation

## SerialLite3

Wrapper around Intel's SerialLite III IP for the Stratix-10 FPGA and
tested on the Terasic DE10Pro board.

 * `s10_seriallite3_4lane_wrapper.v` - wrapper around the SerialLite
   III IP including instantiation of the required transmitter PLLs for
   four lanes (100Gbps links comprising four 25Gbps links bonded together).
 * `s10_seriallite3_4lane.ip` - IP parameters for SerialLite III
 * `xcvr_atx_pll_s10_htile.ip` - IP parameters for the transmitter
   PLLs (two per four channel link)
 * `s10_seriallite3_4lane.qsf` - serial pin definitions including
   specification of analog properties of the transmitter, etc.
 * `SerialLite3.bsv` - BSV wrapper providing interfaces suitable for
   Qsys

# Tutorial Material

## Tuning Serial Links

Analog parameters for serial links can be tuned using System Console.
Please see the demonstration video below.  The tuned analog (PMA)
parameters are included in the DE10Pro.qsf file.

[![Link Tuning Video](https://img.youtube.com/vi/y_UbtNqbIaM/default.jpg)](https://youtu.be/y_UbtNqbIaM)

