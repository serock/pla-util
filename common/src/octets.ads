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
with Ada.Text_IO;
with GNAT.Formatted_String;
with Interfaces.C;

package Octets is

   type Octet_Type is new Interfaces.C.unsigned_char;

   type Octets_Type is array (Positive range <>) of aliased Octet_Type
     with
       Convention => C;

   subtype HFID_Octets_Type is Octets.Octets_Type (1 .. 64);
   subtype Key_Type         is Octets.Octets_Type (1 .. 16);

   package Octet_Text_IO is new Ada.Text_IO.Modular_IO (Num => Octet_Type);

   function Octet_Format is new GNAT.Formatted_String.Mod_Format (Int => Octet_Type,
                                                                  Put => Octet_Text_IO.Put);

end Octets;
