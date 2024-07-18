/*
 * Copyright (c) 2022 Simon W. Moore
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
 * Status Device
 *
 * A simple memory mapped device that allows status bits from a number of
 * 32b conduits, and to read the unique Stratix-10 ChipID.
 * 
 * TODO: how to get error_rx and error_tx captured since the bits change
 * momentarily and at a higher clock frequency (TX and RX clocks)
 */


package StatusDevice;

import Vector     :: *;
import GetPut     :: *;
import DReg       :: *;
import BlueAXI4   :: *;
import BlueBasics :: *;
import ChipID     :: *;


(* always_ready, always_enabled*)
interface StatusConduits #(numeric type n);
  // conduit interfaces
  // TODO: it might be better to have a vector of conduits, though naming might be a challenge
  method Action status_a(Bit#(n) coe);
  method Action status_b(Bit#(n) coe);
  method Action status_c(Bit#(n) coe);
  method Action status_d(Bit#(n) coe);
endinterface


interface StatusDevice#(
  numeric type t_addr
, numeric type t_awuser, numeric type t_wuser, numeric type t_buser
, numeric type t_aruser, numeric type t_ruser
, numeric type t_status_sz
);
  // memory slave for control/status registers
  interface AXI4Lite_Slave #( t_addr, 32
                            , t_awuser, t_wuser, t_buser
                            , t_aruser, t_ruser) mem_csrs;

  interface StatusConduits #(t_status_sz) status;
endinterface



interface StatusDevice_Sig#(
  numeric type t_addr
, numeric type t_awuser, numeric type t_wuser, numeric type t_buser
, numeric type t_aruser, numeric type t_ruser
, numeric type t_status_sz
);
  (* prefix = "axls_mem_csrs" *)
  interface AXI4Lite_Slave_Sig #( t_addr, 32
                                , t_awuser, t_wuser, t_buser
                                , t_aruser, t_ruser) mem_csrs;
  (* prefix = "coe" *)
  interface StatusConduits #(t_status_sz) status;
endinterface


module mkStatusDevice(StatusDevice#(t_addr, t_awuser, t_wuser, t_buser, t_aruser, t_ruser, t_status_sz) ifc)
provisos (Add #(__a, t_status_sz, 32));

  Vector#(4, Reg#(Bit#(t_status_sz))) status_regs <- replicateM(mkReg(0));
  Vector#(4, Reg#(Bit#(t_status_sz))) status_regs_latch <- replicateM(mkReg(0));
  Vector#(4, Reg#(Bool)) zero_status_reg <- replicateM(mkDReg(False));
  Reg#(Bit#(64))                 chip_id <- mkReg(0);
  Reg#(Bool)               chip_id_valid <- mkReg(False);
  Get#(Bit#(64))             get_chip_id <- mkChipID;

  let axiShim <- mkAXI4LiteShimFF;

  rule get_chip_id_once(!chip_id_valid);
    chip_id_valid <= True;
    let result_get_chip_id <- get_chip_id.get();
    chip_id <= result_get_chip_id;
  endrule
  
  for(Integer j=0; j<4; j=j+1)
    rule record_status_errors;
      if(zero_status_reg[j])
	status_regs_latch[j] <= 0;
      else
	status_regs_latch[j] <= status_regs_latch[j] | status_regs[j];
    endrule
  
  rule read_req;
    let r <- get (axiShim.master.ar);
    Bit#(32) d = 32'hdeaddead;
    Bit#(3) word_addr =r.araddr[4:2];
    if(word_addr[2]==0) // word_addr<4
      begin
	d = zeroExtend(status_regs[word_addr[1:0]]);
	zero_status_reg[word_addr[1:0]] <= True;
      end
    if(word_addr==4)
      d = chip_id[31:0];
    if(word_addr==5)
      d = chip_id[63:32];

    let rsp = AXI4Lite_RFlit { rdata: d
                             , rresp: OKAY
                             , ruser: ? };
    axiShim.master.r.put (rsp);
  endrule

  // write requests handling, i.e. always ignnore write and return success
  rule write_req;
    // ignore writes
    // let aw <- get (axiShim.master.aw);
    // let w <- get (axiShim.master.w);
    let rsp = AXI4Lite_BFlit { bresp: OKAY, buser: ? };
    axiShim.master.b.put (rsp);
  endrule

  // interface
  interface mem_csrs = axiShim.slave;
  interface StatusConduits status;
    method Action status_a(Bit#(t_status_sz) coe);  status_regs[0] <= coe;  endmethod
    method Action status_b(Bit#(t_status_sz) coe);  status_regs[1] <= coe;  endmethod
    method Action status_c(Bit#(t_status_sz) coe);  status_regs[2] <= coe;  endmethod
    method Action status_d(Bit#(t_status_sz) coe);  status_regs[3] <= coe;  endmethod
  endinterface
endmodule



module toStatusDevice_Sig#(StatusDevice#(t_addr, t_awuser, t_wuser, t_buser, t_aruser, t_ruser, t_status_sz) ifc)
                  (StatusDevice_Sig#(t_addr, t_awuser, t_wuser, t_buser, t_aruser, t_ruser, t_status_sz));
  let sigAXI4LitePort <- toAXI4Lite_Slave_Sig(ifc.mem_csrs);
  return interface StatusDevice_Sig;
    interface mem_csrs = sigAXI4LitePort;
    interface status = ifc.status;
  endinterface;
endmodule


(* synthesize *)
module mkStatusDevice_Instance(StatusDevice_Sig#(
  // t_addr, t_awuser, t_wuser, t_buser, t_aruser, t_ruser, t_status_sz
          5,        0,       0,       0,        0,       0,          32) sd);
  let sd <- mkStatusDevice();
  let sd_sig <- toStatusDevice_Sig(sd);
  return sd_sig;
endmodule

(* synthesize *)
module mkStatusDevice_Instance_Status15(StatusDevice_Sig#(
  // t_addr, t_awuser, t_wuser, t_buser, t_aruser, t_ruser, t_status_sz
          5,        0,       0,       0,        0,       0,          15) sd);
  let sd <- mkStatusDevice();
  let sd_sig <- toStatusDevice_Sig(sd);
  return sd_sig;
endmodule


endpackage
