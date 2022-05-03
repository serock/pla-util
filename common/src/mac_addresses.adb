------------------------------------------------------------------------
--  pla-util - A powerline adapter utility
--  Copyright (C) 2016-2022 John Serock
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
with Ada.Characters.Handling;
with GNAT.Formatted_String;

use GNAT.Formatted_String;

package body MAC_Addresses is

   function "<" (Left  : MAC_Address_Type;
                 Right : MAC_Address_Type) return Boolean is

      use type Octets.Octet_Type;

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

   function Image (Self : MAC_Address_Type) return MAC_Address_Image_Type is

      Hex_Format : Formatted_String := +"%02x%c%02x%c%02x%c%02x%c%02x%c%02x";
      S          : MAC_Address_Image_Type;

   begin

      for I in Self.Octets'First .. Self.Octets'Last - 1 loop
         Hex_Format := Octets.Octet_Format (Format => Hex_Format,
                                            Var    => Self.Octets (I)) & ':';
      end loop;

      Hex_Format := Octets.Octet_Format (Format => Hex_Format,
                                         Var    => Self.Octets (Self.Octets'Last));

      S := -Hex_Format;

      return S;

   end Image;

   function Is_Unicast (Self : MAC_Address_Type) return Boolean is

      use type Octets.Octet_Type;

   begin

      return (Self.Octets (1) and 1) = 0;

   end Is_Unicast;

   function Value (Image : MAC_Address_Image_Type) return MAC_Address_Type is

      Separator : constant Character := ':';

   begin

      if Image (3) /= Separator or else Image (6) /= Separator or else Image (9) /= Separator or else Image (12) /= Separator or else Image (15) /= Separator then

         raise MAC_Address_Error with "Invalid MAC Address format " & Image;

      end if;

      if Image = "00:00:00:00:00:00" then
         return Null_MAC_Address;
      end if;

      declare

         Lower_Image        : constant MAC_Address_Image_Type := Ada.Characters.Handling.To_Lower (Item => Image);
         MAC_Address_Octets : MAC_Address_Octets_Type;

      begin

         if Lower_Image = "ff:ff:ff:ff:ff:ff" then
            return Broadcast_MAC_Address;
         end if;

         MAC_Address_Octets (1) := Octets.Octet_Type'Value ("16#" & Lower_Image (1 .. 2) & '#');
         MAC_Address_Octets (2) := Octets.Octet_Type'Value ("16#" & Lower_Image (4 .. 5) & '#');
         MAC_Address_Octets (3) := Octets.Octet_Type'Value ("16#" & Lower_Image (7 .. 8) & '#');
         MAC_Address_Octets (4) := Octets.Octet_Type'Value ("16#" & Lower_Image (10 .. 11) & '#');
         MAC_Address_Octets (5) := Octets.Octet_Type'Value ("16#" & Lower_Image (13 .. 14) & '#');
         MAC_Address_Octets (6) := Octets.Octet_Type'Value ("16#" & Lower_Image (16 .. 17) & '#');

         return Create_MAC_Address (Octets => MAC_Address_Octets);

      exception

         when Constraint_Error =>

            raise MAC_Address_Error with "Invalid MAC Address " & Image;

      end;

   end Value;

end MAC_Addresses;
