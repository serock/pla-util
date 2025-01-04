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
with Interfaces.C.Strings;

package body Packets.Filters is

   procedure Apply_To (Self   : Filter_Type;
                       Handle : Pcap.Pcap_Access_Type) is

      use type Interfaces.C.int;

      Return_Code : Interfaces.C.int;

   begin

      Return_Code := Pcap.Set_Filter (P              => Handle,
                                      Filter_Program => Self.Program'Unchecked_Access);

      if Return_Code /= 0 then
         raise Packet_Error with Interfaces.C.Strings.Value (Item => Pcap.Get_Error_Text (P => Handle));
      end if;

   end Apply_To;

   procedure Compile (Self       : in out Filter_Type;
                      Expression :        String;
                      Handle     :        Pcap.Pcap_Access_Type) is

      use type Interfaces.C.int;

      Return_Code : Interfaces.C.int;

   begin

      Return_Code := Pcap.Compile (P                 => Handle,
                                   Filter_Program    => Self.Program,
                                   Filter_Expression => Interfaces.C.To_C (Item => Expression),
                                   Optimize          => 1);

      if Return_Code /= 0 then
         raise Packet_Error with Interfaces.C.Strings.Value (Item => Pcap.Get_Error_Text (P => Handle));
      end if;

   end Compile;

   overriding procedure Finalize (Self : in out Filter_Type) is
   begin

      if Self.Program.Instructions /= null then

         Pcap.Free_Code (Filter_Program => Self.Program'Unchecked_Access);

      end if;

   end Finalize;

end Packets.Filters;
