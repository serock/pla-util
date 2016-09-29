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
with Byte_IO;
with Interfaces;

use type Interfaces.Unsigned_8;

package body Ethernet is

   function Create_MAC_Address(Octet_1 : in Interfaces.Unsigned_8;
                               Octet_2 : in Interfaces.Unsigned_8;
                               Octet_3 : in Interfaces.Unsigned_8;
                               Octet_4 : in Interfaces.Unsigned_8;
                               Octet_5 : in Interfaces.Unsigned_8;
                               Octet_6 : in Interfaces.Unsigned_8) return MAC_Address_Type is

      MAC_Address : MAC_Address_Type;

   begin

      MAC_Address.Octets := (Octet_1, Octet_2, Octet_3, Octet_4, Octet_5, Octet_6, 0, 0);

      return MAC_Address;

   end Create_MAC_Address;

   function To_String(MAC_Address : in MAC_Address_Type;
                      Separator   : in Character := ':') return String is

      S : String(1 .. 17);

   begin

      S :=
        Byte_IO.To_Hex_String(Item => MAC_Address.Octets(1)) & Separator &
        Byte_IO.To_Hex_String(Item => MAC_Address.Octets(2)) & Separator &
        Byte_IO.To_Hex_String(Item => MAC_Address.Octets(3)) & Separator &
        Byte_IO.To_Hex_String(Item => MAC_Address.Octets(4)) & Separator &
        Byte_IO.To_Hex_String(Item => MAC_Address.Octets(5)) & Separator &
        Byte_IO.To_Hex_String(Item => MAC_Address.Octets(6));

      return S;

   end To_String;

   function "<"(Left  : in MAC_Address_Type;
                Right : in MAC_Address_Type) return Boolean is

   begin

      for I in 1 .. 6 loop

         if Left.Octets(I) /= Right.Octets(I) then

            return Left.Octets(I) < Right.Octets(I);

         end if;

      end loop;

      return False;

   end "<";

end Ethernet;
