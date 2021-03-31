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
with Ada.Text_IO;
with GNAT.Formatted_String;
with Interfaces;

use GNAT.Formatted_String;
use type Interfaces.Unsigned_8;

package body Ethernet is

   package Byte_Text_IO is new Ada.Text_IO.Modular_IO(Num => Interfaces.Unsigned_8);

   function Byte_Format is new GNAT.Formatted_String.Mod_Format(Int => Interfaces.Unsigned_8,
                                                                Put => Byte_Text_IO.Put);

   function Create_MAC_Address(Bytes : in MAC_Address_Bytes_Type) return MAC_Address_Type is

      MAC_Address : MAC_Address_Type;

   begin

      MAC_Address.Bytes := (Bytes(1), Bytes(2), Bytes(3), Bytes(4), Bytes(5), Bytes(6), others => 0);

      return MAC_Address;

   end Create_MAC_Address;

   function To_String(MAC_Address : in MAC_Address_Type;
                      Separator   : in Character := ':') return String is

      Hex_Format : Formatted_String := +"%02x%c%02x%c%02x%c%02x%c%02x%c%02x";

      S : String(1 .. 17);

   begin

      for I in 1 .. 5 loop

         Hex_Format := Byte_Format(Format => Hex_Format,
                                   Var => MAC_Address.Bytes(I)) & Separator;

      end loop;

      Hex_Format := Byte_Format(Format => Hex_Format,
                                Var => MAC_Address.Bytes(6));

      S := -Hex_Format;

      return S;

   end To_String;

   function "<"(Left  : in MAC_Address_Type;
                Right : in MAC_Address_Type) return Boolean is

   begin

      for I in 1 .. 6 loop

         if Left.Bytes(I) /= Right.Bytes(I) then

            return Left.Bytes(I) < Right.Bytes(I);

         end if;

      end loop;

      return False;

   end "<";

end Ethernet;
