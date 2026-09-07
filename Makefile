#  SPDX-License-Identifier: GPL-3.0-or-later
#----------------------------------------------------------------------
#  pla-util - A power line adapter utility
#  Copyright (C) 2016-2026 John Serock
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
SHELL ::= /bin/sh
UNAME ::= $(shell uname)
ifeq ($(UNAME), Linux)
	OS ::= linux
else ifeq ($(UNAME), Darwin)
	OS :: = macos
endif

GCC ?= gcc
GNATMAKE ?= gnatmake
GNATBIND ?= gnatbind
GNATLINK ?= gnatlink
GNATMAKEFLAGS ?= -v
ifeq ($(OS), linux)
	GNATBINDFLAGS ?= -v -Es -shared
else ifeq ($(OS), macos)
	GNATBINDFLAGS ?= -v -Es -static
endif
GNATLINKFLAGS ?= -v
ADAFLAGS ?= -g -gnateE -gnato -O3 -gnatn
LDFLAGS ?= -v

prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
datarootdir = $(prefix)/share
datadir = $(datarootdir)

executable = pla-util
ada_main_unit = pla_util
gnatmakeflags = --GCC=$(GCC) --GNATBIND=$(GNATBIND) --GNATLINK=$(GNATLINK) \
		-eS -j0 -I-
gnatbindflags =
gnatlinkflags = -R
adaflags = -gnat12 -gnatW8 -gnateu -gnata \
	   -fPIE -fstack-check -fstack-protector-strong
ldlibs = -lpcap
ifeq ($(OS), linux)
ldflags = -pie -z relro -z now
else ifeq ($(OS), macos)
ldflags = -dead_strip
endif

export ADA_INCLUDE_PATH = ./cli/src:./pla/src:./net/src:./net/src/$(OS):./common/src:./config

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

.PHONY: install
install:
	install ./bin/pla-util $(bindir)
	install -m 0644 \
		./completions/pla-util \
		$(datadir)/bash-completion/completions

.PHONY: uninstall
uninstall:
	-rm $(bindir)/pla-util
	-rm $(datadir)/bash-completion/completions/pla-util

.DEFAULT_GOAL := all
