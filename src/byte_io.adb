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
with Ada.Text_IO;
with Interfaces;

use type Interfaces.Unsigned_8;

package body Byte_IO is

   Hex_Characters : constant array (Interfaces.Unsigned_8 range 0 .. 15) of Character :=
     ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');

   procedure Put_Hex(Item : in Interfaces.Unsigned_8) is

      S : String(1 .. 2) := To_Hex_String(Item => Item);

   begin

      Ada.Text_IO.Put(Item => S);

   end Put_Hex;

   procedure Put_Hex_Line(Item : in Interfaces.Unsigned_8) is

   begin

      Put_Hex(Item => Item);

      Ada.Text_IO.New_Line(Spacing => 1);

   end Put_Hex_Line;

   function To_Hex_String(Item : in Interfaces.Unsigned_8) return String is

      S : String(1 .. 2);

   begin

      S(1) := Hex_Characters(Interfaces.Shift_Right(Value  => Item,
                                                    Amount => 4));

      S(2) := Hex_Characters(Item and 16#0f#);

      return S;

   end To_Hex_String;

end Byte_IO;

