/*
 * Copyright (c) 2023 Simon W. Moore and Alexandre Joannou
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
 * Bluespec wrapper around Intel's SerialLite3 IP instantiated for four
 * channels, i.e. suitable for one of the 100Gbps links on the Stratix-10 based
 * DE10Pro board.
 */

package SerialLite3;

import BlueAXI4        :: *;
import BlueBasics      :: *;
import ConfigReg       :: *;
import Vector          :: *;
import Clocks          :: *;
import FIFO            :: *;
import DReg            :: *;
import S10Synchronizer :: *;

// This package provides types / interfaces for the SerialLite3 interface
// Note: the following signals are not currently provided:
// - method Bool err_interrupt_tx;
// - method Bool err_interrupt_rx;
// - method Action crc_error_inject (Bit #(4) x);

// This flit structure can be used as payload for the rx/tx channels in the
// SerialLite3 interface. Note that the valid / ready signals are abstracted
// away and expected to be handled, here, using Source and Sink interfaces
typedef struct {
  Bit #(256) data;
  Bool start_of_burst;
  Bool end_of_burst;
  Bit #(8) sync;
} SerialLite3_StreamFlit deriving (Bits);

function SerialLite3_StreamFlit axs2sl3 (AXI4Stream_Flit #(0, 256, 0, 9) axs) =
  SerialLite3_StreamFlit { data: axs.tdata
                         , start_of_burst: False  // now derived from end_of_burst
			                          // used to be: axs.tuser[8]==1 // unpack (msb (axs.tuser))
                         , end_of_burst: axs.tlast
                         , sync: truncate (axs.tuser) };

function AXI4Stream_Flit #(0, 256, 0, 9) sl32axs (SerialLite3_StreamFlit sl3) =
  AXI4Stream_Flit { tdata: sl3.data
                  , tstrb: ~0
                  , tkeep: ~0
                  , tlast: sl3.end_of_burst
                  , tid  : ?
                  , tdest: ?
                  , tuser: {pack (sl3.start_of_burst), sl3.sync} };

typedef struct {
   Bit #(2) tx_pll_locked;
   Bit #(2) tx_pll_cal_busy;
   Bit #(5) error_rx;
   Bit #(4) error_tx;
   Bool link_up_tx;
   Bool link_up_rx;
} SerialLite3_LinkStatus deriving (Bits);

// The SerialLite3 external signals
(* always_enabled *) // implies always_ready
interface SerialLite3_ExternalPins;
  method Bit #(4) qsfp28_tx_pins;
  method Action qsfp28_rx_pins (Bit #(4) x);
endinterface

// The SerialLite3 internal signals
interface SerialLite3 #(
  // data to send stream parameters
//  type tx_payload // use SerialLite3_StreamFlit
  // data received stream parameters
//, type rx_payload // use SerialLite3_StreamFlit
  // Physical management memory-mapped AXI4 subordinate port parameters
  numeric type t_addr
, numeric type t_data
, numeric type t_awuser, numeric type t_wuser, numeric type t_buser
, numeric type t_aruser, numeric type t_ruser
);

  // data to send stream
  interface AXI4Stream_Slave #(0, 256, 0, 9) tx;
  // data received stream
  interface AXI4Stream_Master #(0, 256, 0, 9) rx;

  // export receive stream clock
  interface Clock rx_clk;
  interface Reset rx_rst_n;

  // link status
  (* always_ready, always_enabled *)
  method SerialLite3_LinkStatus link_status;

  // Physical management memory-mapped AXI4 subordinate
  // This interface covers the following signals:
  // - phy_mgmt_address
  // - phy_mgmt_read
  // - phy_mgmt_readdata
  // - phy_mgmt_waitrequest
  // - phy_mgmt_write
  // - phy_mgmt_writedata
  interface AXI4Lite_Slave #( t_addr, t_data
                            , t_awuser, t_wuser, t_buser
                            , t_aruser, t_ruser) management_subordinate;

  interface SerialLite3_ExternalPins pins;

endinterface

// The SerialLite3 "Sig" version of the interface
interface SerialLite3_Sig #(
  numeric type t_addr
, numeric type t_data
, numeric type t_awuser, numeric type t_wuser, numeric type t_buser
, numeric type t_aruser, numeric type t_ruser
);

  (* prefix = "axstrs_C_csi_tx_clk_C__R_rsi_tx_rst_n_R__tx" *)
  interface AXI4Stream_Slave_Sig #(0, 256, 0, 9) tx;
  (* prefix = "axstrm_C_cso_rx_clk_C__R_rso_rx_rst_n_R__rx" *)
  interface AXI4Stream_Master_Sig #(0, 256, 0, 9) rx;
  interface Clock rx_clk;
  interface Reset rx_rst_n;
  (* result = "coe_C_csi_clk_C__R_rsi_rst_n_R__link_status"
   , always_ready
   , always_enabled
   *)
  method SerialLite3_LinkStatus link_status;
  (* prefix = "axls_C_csi_clk_C__R_rsi_rst_n_R__management" *)
  interface AXI4Lite_Slave_Sig #( t_addr, t_data
                                , t_awuser, t_wuser, t_buser
                                , t_aruser, t_ruser) management_subordinate;
  (* prefix = "coe_C_csi_clk_C__R_rsi_rst_n_R_" *)
  interface SerialLite3_ExternalPins pins;
endinterface

module toSerialLite3_Sig #(SerialLite3 #(a,b,c,d,e,f,g) ifc
                          , Clock tx_clk, Reset tx_rst_n)
                          (SerialLite3_Sig #(a,b,c,d,e,f,g));
  let sigAXI4LitePort <- toAXI4Lite_Slave_Sig (ifc.management_subordinate);
  let sigTXPort <- toAXI4Stream_Slave_Sig (ifc.tx, clocked_by tx_clk, reset_by tx_rst_n);
  let sigRXPort <- toAXI4Stream_Master_Sig (ifc.rx, clocked_by ifc.rx_clk, reset_by ifc.rx_rst_n);
  return interface SerialLite3_Sig;
    interface tx = sigTXPort;
    interface rx = sigRXPort;
    interface rx_clk = ifc.rx_clk;
    interface rx_rst_n = ifc.rx_rst_n;
    method link_status = ifc.link_status;
    interface management_subordinate = sigAXI4LitePort;
    interface pins = ifc.pins;
  endinterface;
endmodule

// Interface to Stratix 10 SerialLite3 IP block insantiated with 4 lanes
interface S10_SerialLite3_4Lane;
  // export receive stream clock
  interface Clock rx_clk;
  interface Reset rx_rst_n;
  // data received stream
  method Bit#(256) data_rx;
  method Bit#(1) start_of_burst_rx;
  method Bit#(1) end_of_burst_rx;
  method Bit#(8) sync_rx;
  method Bit#(9) error_rx;
  method Bit#(1) error_interrupt_rx;
  method Bit#(1) valid_rx;
  method Bit#(1) link_up_rx;
  method Action rx_drop();
  // data transmit stream
  method Action tx(Bit#(256) data_tx, Bit#(1) start_of_burst_tx, Bit#(1) end_of_burst_tx, Bit#(8) sync_tx);
  method Bit#(4) error_tx;
  method Bit#(1) error_interrupt_tx;
  method Bit#(1) ready_tx;
  method Bit#(1) link_up_tx;
  // memory mapped interface to control/status registers
  method Action bus_request(Bit#(16) addr, Bit#(1) read_enable, Bit#(1) write_enable, Bit#(32) write_data);
  method Bit#(32) bus_read_data();
  method Bit#(1) bus_waitrequest();
  // export high-speed serial pins
  (* always_ready, always_enabled *) method Bit#(4) qsfp28_tx_pins;
  (* always_ready, always_enabled *) method Action qsfp28_rx_pins(Bit#(4) a);
  // export TX PLL lock and calibration status
  (* always_ready, always_enabled *) method Bit#(2) tx_pll_locked;
  (* always_ready, always_enabled *) method Bit#(2) tx_pll_cal_busy;
  // test ports
  (* always_ready, always_enabled *) method Action crc_error_inject(Bit#(4) error_inject);
endinterface


// Clock synchroniser using the Quartus synchronizer (wrapped by mkS10SynchronizerCC).
// It will synchronise on a *per bit* basis, i.e. not suitable for
// synchronising words if all of the bits need to be synchronised in the same cycle.
(* always_enabled, always_ready *)
module mkGenericSynchroniserCC#(Clock sclk, Reset srst_n) (Reg#(a)) provisos (Bits#(a,a_width));
  
  Vector#(a_width, Reg#(Bit#(1))) crossing <- replicateM(mkS10SynchronizerCC(sclk, srst_n));

  // runs in sclk clock domain
  method Action _write(a x);
     Bit#(SizeOf#(a)) b = pack(x);
     for(Integer j=0; j<valueOf(a_width); j=j+1)
       crossing[j] <= b[j];
  endmethod
  
  // runs in default clock domain
  method a _read;
     Bit#(SizeOf#(a)) b=?;
     for(Integer j=0; j<valueOf(a_width); j=j+1)
       b[j] = crossing[j];
     return unpack(b);
  endmethod
endmodule
  
// Every time an input event goes high the output remains high for 2^timersize cycles
// in order to ensure that a status but going from a fast clock domain will be seen
// in a slower clock domain.
//module mkPersistEvent#(numeric type timersize)(Reg#(Bit#(1)) ifc);
module mkPersistEvent(Reg#(Bit#(1)) ifc);
  Reg#(Bit#(1))    d <- mkReg(0);   // data store
  Reg#(Bit#(4))    t <- mkReg(0);   // timer - TODO: make the width of the timer perameterisable
  Reg#(Bit#(1))    e <- mkDReg(0);  // event signal
  
  rule timer;
    if(e==1)
      begin
	d <= 1;
	t <= ~0;
      end
    else
      if(t>0)
	t <= t-1;
    else
      d <= 0;
  endrule
  
  method Action _write(Bit#(1) x);
    e <= x;
  endmethod
  
  method Bit#(1) _read = d;
endmodule  

  
// persist events from faster clocked source to slower destination (default clock domain)
module mkPersistEventClockCrosserCC#(Integer timersize, Clock sclk, Reset srst_n) (Reg#(a)) provisos (Bits#(a, a_width));
  Vector#(a_width, Reg#(Bit#(1)))  persist <- replicateM(mkPersistEvent(clocked_by sclk, reset_by srst_n));
  Vector#(a_width, Reg#(Bit#(1))) crossing <- replicateM(mkS10SynchronizerCC(sclk, srst_n));

  // runs in sclk clock domain
  method Action _write(a x);
     Bit#(SizeOf#(a)) b = pack(x);
     for(Integer j=0; j<valueOf(a_width); j=j+1)
       begin
	 persist[j] <= b[j];
	 crossing[j] <= persist[j];
       end
  endmethod
  
  // runs in default clock domain
  method a _read = unpack(pack(readVReg(crossing)));
endmodule
  
  
import "BVI" s10_seriallite3_4lane_wrapper =
module mkS10_SerialLite3_4Lane (Clock tx_clk, Reset tx_rst_n, Clock qsfp_refclk, S10_SerialLite3_4Lane sl3);
  // Clocks
  default_clock clk (phy_mgmt_clk, (*unused*) phy_mgmt_clk_gate);
  default_reset rst_n (phy_mgmt_clk_reset_n);
  output_clock rx_clk(interface_clock_rx);
  output_reset rx_rst_n(interface_clock_reset_rx_n) clocked_by (rx_clk);
  input_clock (user_clock_tx, (*unused*) user_clock_tx_gate) = tx_clk;
  input_reset tx_rst_n(user_clock_reset_tx_n) clocked_by (tx_clk) = tx_rst_n;
  // High-speed transmitter clock that must be physically connected to the same H-tile as the tx/rx pins
  input_clock (xcvr_pll_ref_clk, (*unused*) xcvr_pll_ref_clk_gate) = qsfp_refclk;

  // Transmit stream
  method tx(data_tx, start_of_burst_tx, end_of_burst_tx, sync_tx)
                 enable (valid_tx) ready (ready_tx) clocked_by(tx_clk) reset_by(tx_rst_n);
  method            error_tx error_tx()             clocked_by(tx_clk) reset_by(tx_rst_n);
  method          link_up_tx link_up_tx()           clocked_by(tx_clk) reset_by(tx_rst_n);
  method    err_interrupt_tx error_interrupt_tx()   clocked_by(tx_clk) reset_by(tx_rst_n);
  method            ready_tx ready_tx()             clocked_by(tx_clk) reset_by(tx_rst_n);

  // Receive stream
  method             data_rx data_rx()              clocked_by(rx_clk) reset_by(rx_rst_n);
  method   start_of_burst_rx start_of_burst_rx()    clocked_by(rx_clk) reset_by(rx_rst_n);
  method     end_of_burst_rx end_of_burst_rx()      clocked_by(rx_clk) reset_by(rx_rst_n);
  method             sync_rx sync_rx()              clocked_by(rx_clk) reset_by(rx_rst_n);

  method            error_rx error_rx()             clocked_by(rx_clk) reset_by(rx_rst_n);
  method          link_up_rx link_up_rx()           clocked_by(rx_clk) reset_by(rx_rst_n);
  method    err_interrupt_rx error_interrupt_rx()   clocked_by(rx_clk) reset_by(rx_rst_n);
  method            valid_rx valid_rx()             clocked_by(rx_clk) reset_by(rx_rst_n);
  method rx_drop() enable(ready_rx) ready(valid_rx) clocked_by(rx_clk) reset_by(rx_rst_n);

  // Memory mapped interface (uses the default clock and reset)
  method bus_request(phy_mgmt_address, phy_mgmt_read, phy_mgmt_write, phy_mgmt_writedata) enable(phy_mgmt_valid);
  method phy_mgmt_readdata bus_read_data();
  method phy_mgmt_waitrequest bus_waitrequest();

  // High-speed serial pins (no clock domain)
  method tx_serial_data qsfp28_tx_pins();
  method qsfp28_rx_pins(rx_serial_data) enable((*inhigh*) EN_rx_serial_data);

  // TX PLLs - lock and calibration status 
  method tx_pll_locked tx_pll_locked() clocked_by(tx_clk) reset_by(tx_rst_n);
  method tx_pll_cal_busy tx_pll_cal_busy() clocked_by(tx_clk) reset_by(tx_rst_n);

  // Test/monitor ports (TODO: correct clock domain?)
  method crc_error_inject(crc_error_inject) enable((*inhigh*) EN_crc_error_inject) clocked_by(tx_clk) reset_by(tx_rst_n);

  // Scheduling
  schedule ( data_rx, start_of_burst_rx, end_of_burst_rx, valid_rx, error_rx, link_up_rx, sync_rx, error_interrupt_rx, rx_drop
           , link_up_rx, link_up_tx, error_tx, error_interrupt_tx, ready_tx
           , bus_request, bus_read_data, bus_waitrequest, crc_error_inject, tx
	   , qsfp28_tx_pins, qsfp28_rx_pins
	   , tx_pll_locked, tx_pll_cal_busy)
        CF ( data_rx, start_of_burst_rx, end_of_burst_rx, valid_rx, error_rx, link_up_rx, sync_rx, error_interrupt_rx, rx_drop
           , link_up_rx, link_up_tx, error_tx, error_interrupt_tx, ready_tx
           , bus_request, bus_read_data, bus_waitrequest, crc_error_inject, tx
	   , qsfp28_tx_pins, qsfp28_rx_pins
	   , tx_pll_locked, tx_pll_cal_busy);
endmodule

module mkSerialLite3
  (
   Clock tx_clk, Reset tx_rst_n, Clock qsfp_refclk,
   SerialLite3#(t_addr, t_data, t_awuser, t_wuser, t_buser, t_aruser, t_ruser) sl3_ifc
  )
  provisos ( Add#(16,_na,t_addr)
           , Add#(32,_nd,t_data)
           , Alias#(t_bus_req, Tuple4#(Bit#(16), Bit#(1), Bit#(1), Bit#(32))) );

  S10_SerialLite3_4Lane             sl3 <- mkS10_SerialLite3_4Lane(tx_clk, tx_rst_n, qsfp_refclk);
  Reg#(Bool)                    tx_wait <- mkDReg(False, clocked_by tx_clk, reset_by tx_rst_n);
  Reg#(Bool)          tx_start_of_burst <- mkReg(True, clocked_by tx_clk, reset_by tx_rst_n);
//  PulseWire                 tx_slowdown <- mkPulseWire(clocked_by tx_clk, reset_by tx_rst_n);
//  Reg#(Bit#(1))       tx_slowdown_timer <- mkConfigReg(0, clocked_by tx_clk, reset_by tx_rst_n);
  Reg#(Bit#(2))      sync_tx_pll_locked <- mkGenericSynchroniserCC(tx_clk, tx_rst_n);
  Reg#(Bit#(2))    sync_tx_pll_cal_busy <- mkGenericSynchroniserCC(tx_clk, tx_rst_n);
  Reg#(Bit#(4))           sync_error_tx <- mkPersistEventClockCrosserCC(8, tx_clk, tx_rst_n);
  Reg#(Bit#(5))           sync_error_rx <- mkPersistEventClockCrosserCC(8, sl3.rx_clk, sl3.rx_rst_n);
  Reg#(Bool)            sync_link_up_tx <- mkPersistEventClockCrosserCC(8, tx_clk, tx_rst_n);
  Reg#(Bool)            sync_link_up_rx <- mkPersistEventClockCrosserCC(8, sl3.rx_clk, sl3.rx_rst_n);
  
  AXI4Lite_Shim#( t_addr, t_data
                , t_awuser, t_wuser, t_buser
                , t_aruser, t_ruser) axi4LiteShim <- mkAXI4LiteShimFF;

  rule do_sync_pll_status;
    sync_tx_pll_locked <= sl3.tx_pll_locked();
    sync_tx_pll_cal_busy <= sl3.tx_pll_cal_busy();
  endrule
  rule do_sync_from_tx_clk_domain;
    sync_link_up_tx <= sl3.link_up_tx==1;
    sync_error_tx <= sl3.error_tx;
  endrule
  rule do_sync_from_rx_clk_domain;
    sync_link_up_rx <= sl3.link_up_rx==1;
    sync_error_rx <= sl3.error_rx[4:0];
  endrule
  rule no_crc_error_testing;
    sl3.crc_error_inject(0);
  endrule

  /*
  FIFO#(t_bus_req) busReqFF <- mkFIFO1; // for bus access from a single rule
  FIFO#(Bit#(0))   rdRspFF <- mkFIFO1; // for flow control
  rule forward_bus_req (sl3.bus_waitrequest == 1'b0);
    match {.addr, .rd, .wr, .data} = busReqFF.first;
    busReqFF.deq;
    sl3.bus_request (addr, rd, wr, data);
    if (rd == 1'b1) rdRspFF.enq(?);
  endrule

  (* descending_urgency = "axi4Lite_read_request, axi4Lite_write" *)
  rule axi4Lite_read_request;
    let arflit <- get (axi4LiteShim.master.ar);
    // TODO assert that the requested size is indeed 32 bits?
    busReqFF.enq (tuple4 (truncate (arflit.araddr), 1'b1, 1'b0, ?));
  endrule
  rule axi4Lite_read_response (sl3.bus_waitrequest == 1'b0);
    rdRspFF.deq;
    axi4LiteShim.master.r.put(AXI4Lite_RFlit { rdata: zeroExtend (sl3.bus_read_data)
                                             , rresp: OKAY
                                             , ruser: ? });
  endrule
  */
  
  FIFO#(t_bus_req) busReqFF <- mkFIFO1; // for bus access from a single rule
  FIFO#(AXI4Lite_RFlit#(t_data, t_ruser)) busRspFF <- mkFIFO1; // pipeline response to meet timing
  Bit#(32) module_version = 32'h2345_2345;
  rule forward_bus_req;
    match {.addr, .rd, .wr, .data} = busReqFF.first;
    if (addr == 0) begin
      busReqFF.deq;
      busRspFF.enq(AXI4Lite_RFlit { rdata: zeroExtend (module_version)
                                  , rresp: OKAY
                                  , ruser: ? });
    end else begin
      sl3.bus_request (addr, rd, wr, data);
      // Note that bus_waitrequest arrives during the clock cycle that rd is enabled
      // so check bus_waitrequest during this clock cycle, so this rule is not atomic
      if ((rd == 1'b1) && (sl3.bus_waitrequest == 1'b0))
        busRspFF.enq(AXI4Lite_RFlit {  rdata: zeroExtend (sl3.bus_read_data)
  				   , rresp: OKAY
  				   , ruser: ? });
  //      axi4LiteShim.master.r.put(AXI4Lite_RFlit { rdata: zeroExtend (sl3.bus_read_data)
  //                                               , rresp: OKAY
  //                                               , ruser: ? });
      if(sl3.bus_waitrequest == 1'b0)
         busReqFF.deq;
  end
  endrule
  rule forward_bus_response;
    axi4LiteShim.master.r.put(busRspFF.first);
    busRspFF.deq;
  endrule

  (* descending_urgency = "axi4Lite_read_request, axi4Lite_write" *)
  rule axi4Lite_read_request;
    let arflit <- get (axi4LiteShim.master.ar);
    // TODO assert that the requested size is indeed 32 bits?
    busReqFF.enq (tuple4 (truncate (arflit.araddr), 1'b1, 1'b0, ?));
  endrule

  rule axi4Lite_write;
    let awflit <- get (axi4LiteShim.master.aw);
    let wflit <- get (axi4LiteShim.master.w);
    // TODO assert that the writen size is indeed 32 bits?
    // TODO consider w.wstrb?
    //      would imply a read first to get the value to merge with?
    //      or maybe assert that it is always 'b1111
    busReqFF.enq (tuple4 (truncate (awflit.awaddr), 1'b0, 1'b1, truncate (wflit.wdata)));
    axi4LiteShim.master.b.put(AXI4Lite_BFlit { bresp: OKAY
                                             , buser: ? });
  endrule
  
  //----------------------------------------------------------------------------
  // interface clocked_by tx_clk reset_by tx_rst_n
  Sink #(SerialLite3_StreamFlit) rawTX = interface Sink;
    method canPut = sl3.ready_tx==1 && !tx_wait;
    method Action put(d) if ((sl3.ready_tx==1) && !tx_wait);
      sl3.tx(d.data, pack(tx_start_of_burst), pack(d.end_of_burst), d.sync);
      // derive start_of_burst based on whether it is the first flit after an end_of_burst
      tx_start_of_burst <= d.end_of_burst;
      if(d.end_of_burst)
	tx_wait <= True; // DReg to pause transmission after every end_of_burst
	//tx_slowdown_timer <= -1;
    endmethod
  endinterface;

  // interface clocked_by rx_clk reset_by rx_rst_n;
  Source #(SerialLite3_StreamFlit) rawRX = interface Source;
    method Bool canPeek = sl3.valid_rx()==1; 
    method SerialLite3_StreamFlit peek() if (sl3.valid_rx==1);
      return SerialLite3_StreamFlit{data:sl3.data_rx(), start_of_burst:sl3.start_of_burst_rx()==1, end_of_burst:sl3.end_of_burst_rx()==1, sync:sl3.sync_rx()};
    endmethod
    method Action drop = sl3.rx_drop;
  endinterface;

  //----------------------------------------------------------------------------

  interface Clock rx_clk = sl3.rx_clk;
  interface Reset rx_rst_n = sl3.rx_rst_n;

  interface SerialLite3_ExternalPins pins;
    method qsfp28_tx_pins = sl3.qsfp28_tx_pins;
    method qsfp28_rx_pins = sl3.qsfp28_rx_pins;
  endinterface

  //----------------------------------------------------------------------------

  interface tx = mapSink (axs2sl3, rawTX);
  interface rx = mapSource (sl32axs, rawRX);

  method SerialLite3_LinkStatus link_status =
    SerialLite3_LinkStatus{tx_pll_locked:sync_tx_pll_locked,
			   tx_pll_cal_busy:sync_tx_pll_cal_busy,
			   error_rx:sync_error_rx,
                           error_tx:sync_error_tx,
                           link_up_tx:sync_link_up_tx,
                           link_up_rx:sync_link_up_rx};

  interface management_subordinate = axi4LiteShim.slave;

endmodule


// Create a stand-alone instance that could be imported into Platform Designer as a component
(* synthesize
 , default_clock_osc = "csi_clk"
 , default_reset = "_C_csi_clk_C_rsi_rst_n"
 , clock_prefix = "cso"
 , gate_prefix = "IGNORE_gate"
 , reset_prefix= "_C_cso_rx_clk_C_rso" *)
module mkSerialLite3_Instance ( (* osc = "csi_tx_clk" *) Clock tx_clk
                              , (* reset = "_C_csi_tx_clk_C_rsi_tx_rst_n" *) Reset tx_rst_n
                              , (* osc = "csi_qsfp_refclk" *) Clock qsfp_refclk
                              , SerialLite3_Sig#(/*SerialLite3_StreamFlit, SerialLite3_StreamFlit,*/
                                                 //  t_addr, t_data, t_awuser, t_wuser, t_buser, t_aruser, t_ruser
                                                         16,     32,     0,        0,       0,       0,        0) sl3);

  let sl3 <- mkSerialLite3(tx_clk, tx_rst_n, qsfp_refclk);
  let sl3_sig <- toSerialLite3_Sig (sl3, tx_clk, tx_rst_n);
  return sl3_sig;

endmodule

endpackage: SerialLite3
