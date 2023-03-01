#-
# Copyright (c) 2018 Alexandre Joannou
# All rights reserved.
#
# This software was developed by SRI International and the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
# DARPA SSITH research programme.
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

BSC = bsc

BLUEAXI4DIR = $(CURDIR)/BlueAXI4
BLUEAXI4DIRS = $(BLUEAXI4DIR):$(BLUEAXI4DIR)/AXI4:$(BLUEAXI4DIR)/AXI4Lite:$(BLUEAXI4DIR)/AXI4Stream
BLUEUNIXBRIDGES = $(BLUEAXI4DIR)/BlueUnixBridges
BLUEBASICSDIR = $(BLUEUNIXBRIDGES)/BlueBasics
BSVPATH = +:$(BLUEAXI4DIRS):$(BLUEUNIXBRIDGES):$(BLUEBASICSDIR)

BSCFLAGS = -p $(BSVPATH)

# generated files directories
BUILDDIR = build
BDIR = $(BUILDDIR)/bdir

REPODIR = S10FPGA
OUTPUTDIR = output

BSCFLAGS += -bdir $(BDIR)

BSCFLAGS += +RTS -K512M -RTS
BSCFLAGS += -show-schedule
BSCFLAGS += -sched-dot
BSCFLAGS += -show-range-conflict
#BSCFLAGS += -show-rule-rel \* \*
#BSCFLAGS += -steps-warn-interval n

SRC_StatusDevice = StatusDevice.bsv
SRC_SerialLite3 = SerialLite3.bsv
SRC_BERT = BERT.bsv

IPSL3 = s10_seriallite3_4lane
IPSL3DIR = $(IPSL3)
IPCHIPID = chipid
IPCHIPIDDIR = $(IPCHIPID)
IPTXPLL = xcvr_atx_pll_s10_htile
IPTXPLLDIR = $(IPTXPLL)

.PHONY: all
all:  mkStatusDevice_Instance mkSerialLite3_Instance mkBERT_Instance

TimeStamp.bsv: timestamp.py
	./timestamp.py > TimeStamp.bsv

mkStatusDevice_Instance: generate_ip_chipid $(SRC_StatusDevice)
	mkdir -p $(OUTPUTDIR)/$@-info $(BDIR)
	$(BSC) -info-dir $(OUTPUTDIR)/$@-info -vdir $(OUTPUTDIR) -opt-undetermined-vals -unspecified-to X $(BSCFLAGS) -verilog -g mkStatusDevice_Instance -u $(SRC_StatusDevice)

mkSerialLite3_Instance: generate_ip_SerialLite3 generate_ip_txpll $(SRC_SerialLite3)
	mkdir -p $(OUTPUTDIR)/$@-info $(BDIR)
	$(BSC) -info-dir $(OUTPUTDIR)/$@-info -vdir $(OUTPUTDIR) -opt-undetermined-vals -unspecified-to X $(BSCFLAGS) -verilog -g mkSerialLite3_Instance -u $(SRC_SerialLite3)

mkBERT_Instance: TimeStamp.bsv $(SRC_BERT)
	mkdir -p $(OUTPUTDIR)/$@-info $(BDIR)
	$(BSC) -info-dir $(OUTPUTDIR)/$@-info -vdir $(OUTPUTDIR) -opt-undetermined-vals -unspecified-to X $(BSCFLAGS) -verilog -g mkBERT_Instance -u $(SRC_BERT)

.PHONY: generate_ip_SerialLite3
generate_ip_SerialLite3: $(IPSL3DIR)/$(IPSL3)_inst.v
$(IPSL3DIR)/$(IPSL3)_inst.v: $(IPSL3).ip
	quartus_ipgenerate -synthesis=verilog --clear_ip_generation_dirs --generate_ip_file --ip_file=$(REPODIR)/$(IPSL3).ip ../DE10_Pro.qsf

.PHONY: generate_ip_chipid
generate_ip_chipid: $(IPCHIPIDDIR)/$(IPCHIPID)_inst.v
$(IPCHIPIDDIR)/$(IPCHIPID)_inst.v: $(IPCHIPID).ip
	quartus_ipgenerate -synthesis=verilog --clear_ip_generation_dirs --generate_ip_file --ip_file=$(REPODIR)/$(IPCHIPID).ip ../DE10_Pro.qsf

.PHONY: generate_ip_txpll
generate_ip_txpll: $(IPTXPLLDIR)/$(IPTXPLL)_inst.v
$(IPTXPLLDIR)/$(IPTXPLL)_inst.v: $(IPTXPLL).ip
	quartus_ipgenerate -synthesis=verilog --clear_ip_generation_dirs --generate_ip_file --ip_file=$(REPODIR)/$(IPTXPLL).ip ../DE10_Pro.qsf


.PHONY: clean
clean:
	rm -f -r $(BUILDDIR) $(IPSL3DIR) $(IPCHIPIDDIR) $(IPTXPLLDIR)

.PHONY: mrproper
mrproper: clean
	rm -f -r $(OUTPUTDIR)
