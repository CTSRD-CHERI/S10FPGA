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
 * Summary:
 * Wrapper around the SerialLite III IP and associated transmitter
 * PLLs for the Stratix 10 FPGA, specifically the DE10pro board from
 * Terasic that supports four 100Gbps links (four 25Gbps links bonded
 * together on each QSFP+ port). 
 * 
 * Timing:
 * Serial Lite III advanced clocking scheme requires the following (from the ug_slite3_streaming.pdf manual):
 * to be placed in the projet QSF file (not the SDC file!)
 *  set_instance_assignment -name GLOBAL_SIGNAL OFF -to *seriallite_iii_streaming*clock_gen:sink_clock_gen|dp_sync:coreclkin_reset_sync|dp_sync_regstage:dp_sync_stage_2*o*
 */

module s10_seriallite3_4lane_wrapper
  (
   input  wire 	       user_clock_tx, //            user_clock_tx.clk
   input  wire 	       user_clock_reset_tx_n, //      user_clock_reset_tx.reset_n
   input  wire [255:0] data_tx, //                  data_tx.tx_data
   input  wire [0:0]   valid_tx, //                 valid_tx.tx_valid
   output wire [0:0]   ready_tx, //                 ready_tx.tx_ready
   input  wire [0:0]   start_of_burst_tx, //        start_of_burst_tx.tx_start_of_burst
   input  wire [0:0]   end_of_burst_tx, //          end_of_burst_tx.tx_end_of_burst
   output wire 	       link_up_tx, //               link_up_tx.tx_link_up
   output wire [3:0]   error_tx, //                 error_tx.tx_error
   output wire 	       interface_clock_rx, //       interface_clock_rx.clk
   output wire 	       interface_clock_reset_rx_n, // interface_clock_reset_rx.reset_n
   output wire [255:0] data_rx, //                  data_rx.rx_data
   output wire [0:0]   valid_rx, //                 valid_rx.rx_valid
   output wire [7:0]   sync_rx, //                  sync_rx.rx_sync
   input  wire [0:0]   ready_rx, //                 ready_rx.rx_ready
   output wire [0:0]   start_of_burst_rx, //        start_of_burst_rx.rx_start_of_burst
   output wire [0:0]   end_of_burst_rx, //          end_of_burst_rx.rx_end_of_burst
   output wire 	       link_up_rx, //               link_up_rx.rx_link_up
   output wire [8:0]   error_rx, //                 error_rx.rx_error
   input  wire 	       phy_mgmt_clk, //             phy_mgmt_clk.clk
   input  wire 	       phy_mgmt_clk_reset_n, //       phy_mgmt_clk_reset.reset_n
   input  wire [3:0]   tx_serial_clk, //            tx_serial_clk.clk
   input  wire 	       xcvr_pll_ref_clk, //         xcvr_pll_ref_clk.clk
   input  wire [15:0]  phy_mgmt_address, //                 phy_mgmt.address
   input  wire 	       phy_mgmt_read, //                         .read
   output wire [31:0]  phy_mgmt_readdata, //                         .readdata
   output wire 	       phy_mgmt_waitrequest, //                         .waitrequest
   input  wire 	       phy_mgmt_valid, // SWM added since valid signal provided by Bluespec
   input  wire 	       phy_mgmt_write, //                         .write
   input  wire [31:0]  phy_mgmt_writedata, //                         .writedata
   output wire 	       err_interrupt_tx, //         err_interrupt_tx.irq
   output wire 	       err_interrupt_rx, //         err_interrupt_rx.irq
   input  wire [3:0]   crc_error_inject, //         crc_error_inject.tx_err_ins
   output wire [3:0]   tx_serial_data, //           tx_serial_data.tx_serial_data
   input  wire [3:0]   rx_serial_data, //           rx_serial_data.rx_serial_data
   input  wire [7:0]   sync_tx,         //                  sync_tx.tx_sync
   output wire [1:0]   tx_pll_locked,
   output wire [1:0]   tx_pll_cal_busy
   );

   wire        interface_clock_reset_rx;
   assign      interface_clock_reset_rx_n = ~interface_clock_reset_rx;
   wire  [1:0] tx_pll_clk_gtx;        

   s10_seriallite3_4lane sl3
     (
      .user_clock_tx            (user_clock_tx),            //   input,    width = 1,            user_clock_tx.clk
      .user_clock_reset_tx      (~user_clock_reset_tx_n),   //   input,    width = 1,      user_clock_reset_tx.reset
      .data_tx                  (data_tx),                  //   input,  width = 256,                  data_tx.tx_data
      .valid_tx                 (valid_tx),                 //   input,    width = 1,                 valid_tx.tx_valid
      .ready_tx                 (ready_tx),                 //  output,    width = 1,                 ready_tx.tx_ready
      // SWM: start_of_burst_tx and end_of_burst_tx should only be high for 1 clock cycle and when there is valid data
      .start_of_burst_tx        (start_of_burst_tx && valid_tx),        //   input,    width = 1,        start_of_burst_tx.tx_start_of_burst
      .end_of_burst_tx          (end_of_burst_tx && valid_tx),          //   input,    width = 1,          end_of_burst_tx.tx_end_of_burst
      .link_up_tx               (link_up_tx),               //  output,    width = 1,               link_up_tx.tx_link_up
      .error_tx                 (error_tx),                 //  output,    width = 4,                 error_tx.tx_error
      .interface_clock_rx       (interface_clock_rx),       //  output,    width = 1,       interface_clock_rx.clk
      .interface_clock_reset_rx (interface_clock_reset_rx), //  output,    width = 1, interface_clock_reset_rx.reset
      .data_rx                  (data_rx),                  //  output,  width = 256,                  data_rx.rx_data
      .valid_rx                 (valid_rx),                 //  output,    width = 1,                 valid_rx.rx_valid
      .sync_rx                  (sync_rx),                  //  output,    width = 8,                  sync_rx.rx_sync
      .ready_rx                 (ready_rx),                 //   input,    width = 1,                 ready_rx.rx_ready
      .start_of_burst_rx        (start_of_burst_rx),        //  output,    width = 1,        start_of_burst_rx.rx_start_of_burst
      .end_of_burst_rx          (end_of_burst_rx),          //  output,    width = 1,          end_of_burst_rx.rx_end_of_burst
      .link_up_rx               (link_up_rx),               //  output,    width = 1,               link_up_rx.rx_link_up
      .error_rx                 (error_rx),                 //  output,    width = 9,                 error_rx.rx_error
      .phy_mgmt_clk             (phy_mgmt_clk),             //   input,    width = 1,             phy_mgmt_clk.clk
      .phy_mgmt_clk_reset       (~phy_mgmt_clk_reset_n),       //   input,    width = 1,       phy_mgmt_clk_reset.reset
      .tx_serial_clk            ({tx_pll_clk_gtx[1],tx_pll_clk_gtx[1],tx_pll_clk_gtx[0],tx_pll_clk_gtx[0]}),
      .tx_pll_locked            (&tx_pll_locked),            //   input,    width = 1,            tx_pll_locked.pll_locked
      .xcvr_pll_ref_clk         (xcvr_pll_ref_clk),         //   input,    width = 1,         xcvr_pll_ref_clk.clk
      .phy_mgmt_address         (phy_mgmt_address[15:2]),         //   input,   width = 14,                 phy_mgmt.address
      .phy_mgmt_read            (phy_mgmt_read && phy_mgmt_valid),            //   input,    width = 1,                         .read
      .phy_mgmt_readdata        (phy_mgmt_readdata),        //  output,   width = 32,                         .readdata
      .phy_mgmt_waitrequest     (phy_mgmt_waitrequest),     //  output,    width = 1,                         .waitrequest
      .phy_mgmt_write           (phy_mgmt_write && phy_mgmt_valid),           //   input,    width = 1,                         .write
      .phy_mgmt_writedata       (phy_mgmt_writedata),       //   input,   width = 32,                         .writedata
      .err_interrupt_tx         (err_interrupt_tx),         //  output,    width = 1,         err_interrupt_tx.irq
      .err_interrupt_rx         (err_interrupt_rx),         //  output,    width = 1,         err_interrupt_rx.irq
      .crc_error_inject         (crc_error_inject),         //   input,    width = 4,         crc_error_inject.tx_err_ins
      .tx_serial_data           (tx_serial_data),           //  output,    width = 4,           tx_serial_data.tx_serial_data
      .rx_serial_data           (rx_serial_data),           //   input,    width = 4,           rx_serial_data.rx_serial_data
      .sync_tx                  (sync_tx)                   //   input,    width = 8,                  sync_tx.tx_sync
      );

   // two PLLs to provide clocks to the four transmitters that are
   // phsycally embedded in the high speed serial-link
   xcvr_atx_pll_s10_htile tx_pll_0
     (
      .pll_refclk0       (xcvr_pll_ref_clk),   // reference clock on PCB that is physically close to serial-link  
      .tx_serial_clk     (),                   // unused clock output						  
      .tx_serial_clk_gxt (tx_pll_clk_gtx[0]),  // output clock							  
      .pll_locked        (tx_pll_locked[0]),   // output PLL lock						  
      .pll_cal_busy      (tx_pll_cal_busy[0])  // output calibration busy
      );
   xcvr_atx_pll_s10_htile tx_pll_1
     (
      .pll_refclk0       (xcvr_pll_ref_clk),   // reference clock on PCB that is physically close to serial-link  
      .tx_serial_clk     (),                   // unused clock output						  
      .tx_serial_clk_gxt (tx_pll_clk_gtx[1]),  // output clock							  
      .pll_locked        (tx_pll_locked[1]),   // output PLL lock						  
      .pll_cal_busy      (tx_pll_cal_busy[1]) // output calibration busy
      );

endmodule

