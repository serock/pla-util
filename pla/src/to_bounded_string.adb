--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016-2023 John Serock
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
with Ada.Characters.Latin_1;
with Ada.Strings;

function To_Bounded_String (Data : Octets.Octets_Type) return T is

   C       : Character;
   Bounded : T;
   I       : Natural := Data'First;

begin

   while I <= Data'Last loop

      C := Character'Val (Data (I));

      if C = Ada.Characters.Latin_1.NUL then
         exit;
      end if;

      Append (S => Bounded,
              C => C);

      I := I + 1;

   end loop;

   return Bounded;

end To_Bounded_String;
