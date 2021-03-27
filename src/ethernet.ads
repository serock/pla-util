------------------------------------------------------------------------
--  pla-util - A powerline adapter utility
--  Copyright (C) 2016-2021 John Serock
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

   type Bytes_Type is array (Positive range <>) of Interfaces.Unsigned_8;

   subtype MAC_Address_Bytes_Type is Bytes_Type(1 .. 6);

   type MAC_Address_Type is tagged private;

   Broadcast_Address : constant MAC_Address_Type;
   Null_Address      : constant MAC_Address_Type;

   function Create_MAC_Address(Bytes : in MAC_Address_Bytes_Type) return MAC_Address_Type;

   function To_String(MAC_Address : in MAC_Address_Type;
                      Separator   : in Character := ':') return String;

   function "<"(Left  : in MAC_Address_Type;
                Right : in MAC_Address_Type) return Boolean;

private

   subtype Long_MAC_Address_Bytes_Type is Bytes_Type(1 .. 8);

   type MAC_Address_Type is tagged
      record
         Bytes : Long_MAC_Address_Bytes_Type;
      end record;

   Broadcast_Address : constant MAC_Address_Type := MAC_Address_Type'(Bytes => (1 .. 6 => 16#ff#, others => 16#00#));
   Null_Address      : constant MAC_Address_Type := MAC_Address_Type'(Bytes => (others => 16#00#));

end Ethernet;
