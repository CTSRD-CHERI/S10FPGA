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
 * Wraps synchronizer library provided by the Quartus tools.
 *   22.1pro/quartus/libraries/megafunctions/altera_std_synchronizer.v
 * Note that the Quartus component contains directives that are needed for
 * timing closure.
 * A Reg interface is used bit in fact four DFFs will be used to synchronise
 * the _write from the src_clk domain to the _read in the default clock domain.
 * The first DFF is in the src_clk domain followed by three DFFs as a
 * synchronizer into the default clock domain.
 */

package S10Synchronizer;

import "BVI" s10synchronizer =
  module mkS10SynchronizerCC(Clock src_clk, Reset src_rst_n, Reg#(Bit#(1)) ifc);
    // destination clock and reset
    default_clock dest_clk (dclk, (*unused*) dclk_gate);
    default_reset dest_rst_n (drst_n);
    // source clock and reset
    input_clock (sclk, (*unused*) sclk_gate) = src_clk;
    input_reset src_rst_n(srst_n) clocked_by (src_clk) = src_rst_n;
    // write in the src_clk domain
    method _write(din) enable(din_en) clocked_by(src_clk) reset_by(src_rst_n);
    // read in the default clock domain
    method dout _read();
    schedule ( _write, _read ) CF ( _write, _read);
  endmodule

endpackage
