/*
 * Copyright (c) 2023 Simon W. Moore
 * All rights reserved.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 *
 * ----------------------------------------------------------------------------
 *
 * Summary
 * Snoop on I2C/SMBus traffic
 */


package I2CSnoop;


import S10Synchronizer :: *;
import FIFO            :: *;
import Clocks          :: *;

typedef struct {
   Bit#(7) addr;
   Bool read;
   Bit#(8) data0;
   Bit#(8) data1;
   } I2CFlit deriving (Bits, Eq);


interface I2CSnoop;
  (* always_enabled *)
  method Action i2csig(Bit#(1) datIn, Bit#(1) clkIn);
  (* always_enabled *)
  method Bit#(1) i2csig_datOutEn;
  (* always_enabled *)
  method Bit#(1) i2csig_clkOutEn;
  method ActionValue#(I2CFlit) received;
  // interface AXI4Stream_Master #(0, 24, 0, 0) rxstream;
endinterface

typedef enum { AckIdle, AckStart, AckWaitClk1, AckWaitClk0 } AckStates deriving (Bits, Eq);

(* synthesize *)
module mkI2CSnoop(I2CSnoop ifc);
  let clk <- exposeCurrentClock;
  let rst <- exposeCurrentReset;
  Reg#(Bit#(1))  sync_datIn <- mkS10SynchronizerCC(clk, rst);
  Reg#(Bit#(1))  sync_clkIn <- mkS10SynchronizerCC(clk, rst); // I2C "clock" to be sampled
  Reg#(Bit#(1))  prev_datIn <- mkReg(0);
  Reg#(Bit#(1))  prev_clkIn <- mkReg(0);
  Reg#(Bit#(1))      datOut <- mkReg(1);
  Reg#(Bit#(1))      clkOut <- mkReg(1); // currently unused
  Reg#(Bit#(24))    shiftIn <- mkReg(0);
  Reg#(Int#(6))       state <- mkReg(-1);
  Reg#(AckStates)     doAck <- mkReg(AckIdle);
  Reg#(Bit#(10)) ackTimeout <- mkReg(0);
  FIFO#(I2CFlit)      flits <- mkFIFO;
  //  AXI4Stream_Shim#(0, 32, 0, 0)                  rxfifo <- mkAXI4StreamShimUGSizedFIFOF32();

  rule receiver;
    prev_datIn <= sync_datIn;
    prev_clkIn <= sync_clkIn;
    Bool datInRising = (prev_datIn==0) && (sync_datIn==1);
    Bool clkInRising = (prev_clkIn==0) && (sync_clkIn==1);
    Bool idleState = (state<0);
    Bool startBit = idleState && (sync_datIn==0) && (sync_clkIn==1);
    Bool sampleBit = !idleState && clkInRising;
    Bool stopBit = !idleState && (sync_clkIn==1) && datInRising;
    if(doAck==AckStart)
      begin
	datOut <= sync_clkIn; // signal Ack (datOut=0) once clk is low
	if(sync_clkIn==0)
	  doAck <= AckWaitClk1;
      end
    else if(doAck==AckWaitClk1)
      begin
	if(sync_clkIn==1) // host has latched the ack
	  begin
	    doAck <= AckWaitClk0;
	    ackTimeout <= ~0;
	  end
      end
    else if(doAck==AckWaitClk0)
      begin
	ackTimeout <= ackTimeout-1;
	if((sync_clkIn==0) || (ackTimeout==0))
	  begin
	    doAck  <= AckIdle;
	    datOut <= 1;
	  end
      end
    else if(startBit)
      begin
	state <= 23;
	shiftIn <= 0;
      end
    else if(stopBit)
      begin
	flits.enq(unpack(shiftIn));
	// if(rxfifo.slave.canPit()) rxfifo.slave.put(shiftIn);
	state <= -1;
      end
    else if(sampleBit)
      begin
	shiftIn[state] <= sync_datIn;
	if(pack(state)[2:0]==0)  // byte received so send ack
	  // && (shiftIn[23:21]!=7))  // HACK to ignore addresses 0x7X
	  doAck <= AckStart;
	state <= state-1;
      end
  endrule

  method Action i2csig(datIn, clkIn);
    sync_datIn <= datIn;
    sync_clkIn <= clkIn;
  endmethod
  method i2csig_datOutEn = 1-datOut; // enable is inverse of data
  method i2csig_clkOutEn = 1-clkOut; // enable is inverse of clock

  method ActionValue#(I2CFlit) received;
    flits.deq;
    return flits.first;
  endmethod
  // interface rxstream = rxfifo.master;  
endmodule



endpackage
