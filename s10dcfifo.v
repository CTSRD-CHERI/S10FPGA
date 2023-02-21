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
 * Wrapper around Intel's dual-clock FIFO (dcfifo) primitive in none pipeline
 * mode.  It has the same interface as the Bluespec standard SyncFIFOIfc.
 * 
 * TODO: also turn off overflow and underflow checking since it should be
 * unnecessary?
 */


// synopsys translate_off
`timescale 1 ps / 1 ps
// synopsys translate_on
module  s10dcfifo 
  (
   data,
   rdclk,
   rdrst_n, // read reset ignored but makes BSV wrapper simpler
   rdreq,
   wrclk,
   wrrst_n,
   wrreq,
   q,
   rdempty_n,
   wrfull_n);

   parameter WIDTH=1;
   parameter DEPTH=2;

   input  [WIDTH-1:0] data;
   input  	      rdclk;
   input  	      rdrst_n;
   input  	      rdreq;
   input  	      wrclk;
   input              wrrst_n;
   input   	      wrreq;
   output [WIDTH-1:0] q;
   output 	      rdempty_n;
   output 	      wrfull_n;

   wire 	      rdempty;
   wire 	      wrfull;
   assign rdempty_n = !rdempty;
   assign wrfull_n  = !wrfull;
   
/*
 `ifndef ALTERA_RESERVED_QIS
// synopsys translate_off
`endif
    tri0     aclr;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_on
`endif
*/

   // synchronise the writer reset (wrrst_n) to reader clock to avoid timing violations
   wire 	      reset;
   altera_reset_synchronizer
        #(
            .DEPTH      (2),
            .ASYNC_RESET(0)
        )
        alt_rst_req_sync_uq1
        (
            .clk        (rdclk),
            .reset_in   (!wrrst_n),
            .reset_out  (reset)
        );

   
   dcfifo  dcfifo_component
     (
      .aclr (reset),
      //.aclr (!wrrst_n),
      .data (data),
      .rdclk (rdclk),
      .rdreq (rdreq),
      .wrclk (wrclk),
      .wrreq (wrreq),
      .q (q),
      .rdempty (rdempty),
      .wrfull (wrfull),
      .eccstatus (),
      .rdfull (),
      .rdusedw (),
      .wrempty (),
      .wrusedw ());
    defparam
        dcfifo_component.enable_ecc  = "FALSE",
        dcfifo_component.intended_device_family  = "Stratix 10",
        dcfifo_component.lpm_hint  = "DISABLE_DCFIFO_EMBEDDED_TIMING_CONSTRAINT=TRUE",
        dcfifo_component.lpm_numwords  = DEPTH,
        dcfifo_component.lpm_showahead  = "ON", // showahead allows "first" method but slows design
        dcfifo_component.lpm_type  = "dcfifo",
        dcfifo_component.lpm_width  = WIDTH,
        dcfifo_component.lpm_widthu  = 5,
        dcfifo_component.overflow_checking  = "ON",
        dcfifo_component.rdsync_delaypipe  = 4,
        dcfifo_component.read_aclr_synch  = "OFF",
        dcfifo_component.underflow_checking  = "ON",
        dcfifo_component.use_eab  = "ON",
        dcfifo_component.write_aclr_synch  = "ON",
        dcfifo_component.wrsync_delaypipe  = 4;

endmodule


