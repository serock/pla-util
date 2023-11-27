#  SPDX-License-Identifier: GPL-3.0-or-later
#----------------------------------------------------------------------
#  pla-util - A power line adapter utility
#  Copyright (C) 2016-2023 John Serock
#
#  This file is part of pla-util.
#
#  pla-util is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  pla-util is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program. If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------------
SHELL = /bin/sh
GCC ?= gcc
GNATMAKE ?= gnatmake
GNATBIND ?= gnatbind
GNATLINK ?= gnatlink
GNATMAKEFLAGS ?= -v
GNATBINDFLAGS ?= -v -Es
GNATLINKFLAGS ?= -v
ADAFLAGS ?= -g -gnateE -gnato -O3 -gnatn
LDFLAGS ?= -v

executable = pla-util
ada_main_unit = pla_util
gnatmakeflags = --GCC=$(GCC) --GNATBIND=$(GNATBIND) --GNATLINK=$(GNATLINK) \
		-eS -j0 -I-
gnatbindflags = -shared
gnatlinkflags = -R
adaflags = -gnat12 -gnateu -gnata -gnatE \
	   -fPIE -fstack-check -fstack-protector-strong
ldlibs = -lpcap
ldflags = -pie -z relro -z now

export ADA_INCLUDE_PATH = ./cli/src:./pla/src:./net/src:./common/src:./config

.SUFFIXES:
.SUFFIXES: .adb .ads .ali .o

.PHONY: clean
clean:
	rm -f ./bin/* ./obj/*

.PHONY: all
all:
	-mkdir ./obj
	-mkdir ./bin
	$(GNATMAKE) $(gnatmakeflags) $(GNATMAKEFLAGS) \
		-D ./obj -o ./bin/$(executable) $(ada_main_unit) \
		-cargs $(adaflags) $(ADAFLAGS) \
		-bargs $(gnatbindflags) $(GNATBINDFLAGS) \
		-largs $(gnatlinkflags) $(GNATLINKFLAGS) \
		$(ldflags) $(LDFLAGS) $(ldlibs) $(LDLIBS)

.DEFAULT_GOAL := all
