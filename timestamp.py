#!/usr/bin/env python3
##############################################################################
# Copyright (c) 2023 Simon W. Moore
# All rights reserved.
# License: BSD-2-Clause
##############################################################################
# Hack: generate Bluespec library containing a timestamp as a 64b hex number
# (i.e. BCD) with digits representing: YYYYMMDDHHMMSS
# i.e. (year, month, day, hours, minutes, seconds)
# Use: regenerate this code everytime a Bluespec system is rebuilt and
# include the 64b timestamp so that it is checkable at runtime

from datetime import date, time, datetime

current_date = datetime.now()

# Generate a Bluespec function with a timestamp that can be included
# in the design we want to ensure is built
print("function Bit#(64) timestamp() = 64'h%s;"
      % (current_date.strftime("%Y%m%d%H%M%S")))
exit(0)

