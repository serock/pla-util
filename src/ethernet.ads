------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016 John Serock
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program. If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------
with Interfaces;

package Ethernet is

   type MAC_Address_Type is tagged private;

   Broadcast_Address : constant MAC_Address_Type;
   Null_Address      : constant MAC_Address_Type;

   function Create_MAC_Address(Octet_1 : in Interfaces.Unsigned_8;
                               Octet_2 : in Interfaces.Unsigned_8;
                               Octet_3 : in Interfaces.Unsigned_8;
                               Octet_4 : in Interfaces.Unsigned_8;
                               Octet_5 : in Interfaces.Unsigned_8;
                               Octet_6 : in Interfaces.Unsigned_8) return MAC_Address_Type;

   function To_String(MAC_Address : in MAC_Address_Type;
                      Separator   : in Character := ':') return String;

   function "<"(Left  : in MAC_Address_Type;
                Right : in MAC_Address_Type) return Boolean;

private

   type Octets_Type is array (1 .. 8) of Interfaces.Unsigned_8;

   type MAC_Address_Type is tagged
      record
         Octets : Octets_Type;
      end record;

   Broadcast_Address : constant MAC_Address_Type := MAC_Address_Type'(Octets => (1 .. 6 => 16#ff#, others => 16#00#));
   Null_Address      : constant MAC_Address_Type := MAC_Address_Type'(Octets => (others => 16#00#));

end Ethernet;
