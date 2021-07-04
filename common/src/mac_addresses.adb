------------------------------------------------------------------------
--  pla-util - A powerline adapter utility
--  Copyright (C) 2021 John Serock
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
with GNAT.Formatted_String;
with Interfaces;

use GNAT.Formatted_String;
use Interfaces;

package body MAC_Addresses is

   function "<" (Left  : MAC_Address_Type;
                 Right : MAC_Address_Type) return Boolean is

   begin

      for I in MAC_Address_Octets_Type'Range loop

         if Left.Octets (I) /= Right.Octets (I) then
            return Left.Octets (I) < Right.Octets (I);
         end if;

      end loop;

      return False;

   end "<";

   function Create_MAC_Address (Octets : MAC_Address_Octets_Type) return MAC_Address_Type is

      MAC_Address : MAC_Address_Type;

   begin

      MAC_Address.Octets := Octets;

      return MAC_Address;

   end Create_MAC_Address;

   function Get_Octets (Self : MAC_Address_Type) return MAC_Address_Octets_Type is

   begin

      return Self.Octets;

   end Get_Octets;

   function Image (Self      : MAC_Address_Type;
                   Separator : Character := ':') return String is

      Hex_Format : Formatted_String := +"%02x%c%02x%c%02x%c%02x%c%02x%c%02x";
      S          : String (1 .. 17);

   begin

      for I in Self.Octets'First .. Self.Octets'Last - 1 loop
         Hex_Format := Octet_Format (Format => Hex_Format,
                                     Var    => Self.Octets (I)) & Separator;
      end loop;

      Hex_Format := Octet_Format (Format => Hex_Format,
                                  Var    => Self.Octets (Self.Octets'Last));

      S := -Hex_Format;

      return S;

   end Image;

end MAC_Addresses;
