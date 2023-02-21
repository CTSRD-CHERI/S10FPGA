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
 * Wraps synchronizer provided by the Quartus tools.
 * 22.1pro/quartus/libraries/megafunctions/altera_std_synchronizer.v
 */

module s10synchronizer
  (
   input  sclk,
   input  srst_n,
   input  dclk,
   input  drst_n,
   input  din,
   input  din_en,
   output dout
   );
   
   reg 	  rdin;
   always @(posedge sclk or posedge srst_n)
     if(!srst_n)
       rdin <= 1'b0;
     else if(din_en)
       rdin <= din;

   altera_std_synchronizer altsync
     (
      .clk(dclk),
      .reset_n(drst_n),
      .din(rdin),
      .dout(dout)
      );
endmodule // s10synchronizer
