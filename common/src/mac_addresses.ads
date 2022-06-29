--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016-2022 John Serock
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
with Octets;

package MAC_Addresses is

   subtype MAC_Address_Octets_Type is Octets.Octets_Type (1 .. 6);

   type MAC_Address_Type is tagged private;

   subtype MAC_Address_Image_Type is String (1 .. 17);

   function "<" (Left  : MAC_Address_Type;
                 Right : MAC_Address_Type) return Boolean;

   function Create_MAC_Address (MAC_Address_Octets : MAC_Address_Octets_Type) return MAC_Address_Type;

   function Get_Octets (Self : MAC_Address_Type) return MAC_Address_Octets_Type;

   function Image (Self : MAC_Address_Type) return MAC_Address_Image_Type;

   function Is_Unicast (Self : MAC_Address_Type) return Boolean;

   function Value (MAC_Address_Image : MAC_Address_Image_Type) return MAC_Address_Type;

   Broadcast_MAC_Address : constant MAC_Address_Type;
   Null_MAC_Address      : constant MAC_Address_Type;

   MAC_Address_Error : exception;

private

   type MAC_Address_Type is tagged
      record
         Octets : MAC_Address_Octets_Type;
      end record;

   Broadcast_MAC_Address : constant MAC_Address_Type := (Octets => (others => 16#ff#));
   Null_MAC_Address      : constant MAC_Address_Type := (Octets => (others => 16#00#));

end MAC_Addresses;
