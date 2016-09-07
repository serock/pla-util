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
package body Ethernet is

   use type Interfaces.Unsigned_8;

   Hex_Characters : constant array (Interfaces.Unsigned_8 range 0 .. 15) of Character :=
     ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');

   function To_String(Octet : in Interfaces.Unsigned_8) return String is

      S : String(1 .. 2);

   begin

      S(1) := Hex_Characters(Interfaces.Shift_Right(Value  => Octet,
                                                    Amount => 4));

      S(2) := Hex_Characters(Octet and 16#0f#);

      return S;

   end To_String;

   function To_String(MAC_Address : in MAC_Address_Type;
                      Separator   : in Character := ':') return String is

      S : String(1 .. 17);

   begin

      S :=
        To_String(Octet => MAC_Address.Octets(1)) & Separator &
        To_String(Octet => MAC_Address.Octets(2)) & Separator &
        To_String(Octet => MAC_Address.Octets(3)) & Separator &
        To_String(Octet => MAC_Address.Octets(4)) & Separator &
        To_String(Octet => MAC_Address.Octets(5)) & Separator &
        To_String(Octet => MAC_Address.Octets(6));

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
