--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016-2025 John Serock
--
--  This file is part of pla-util.
--
--  pla-util is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  pla-util is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program. If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------
with Packets.Pcap;

private with Ada.Finalization;
private with Interfaces.C;

private package Packets.Filters is

   type Filter_Type is tagged limited private;

   procedure Apply_To (Self   : Filter_Type;
                       Handle : Pcap.Pcap_Access_Type);

   procedure Compile (Self       : in out Filter_Type;
                      Expression :        String;
                      Handle     :        Pcap.Pcap_Access_Type)
     with
       Pre => Self.Is_Available;

   function Is_Available (Self : Filter_Type) return Boolean;

private

   use type Interfaces.C.unsigned;
   use type Pcap.BPF_Instruction_Access_Type;

   type Filter_Type is new Ada.Finalization.Limited_Controlled with
      record

         Program : aliased Pcap.BPF_Program_Type;

      end record;

   overriding procedure Finalize (Self : in out Filter_Type)
     with
       Post => Self.Is_Available;

   function Is_Available (Self : Filter_Type) return Boolean is
      (Self.Program.Length = 0 and then Self.Program.Instructions = null);

end Packets.Filters;
