--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016-2026 John Serock
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
package Packets.Pcap.Link_Layer_Addresses is

   type Socket_Link_Layer_Address_Type is private;

   type Socket_Link_Layer_Address_Access_Type is access Socket_Link_Layer_Address_Type;

   function Is_Not_Link_Layer_Address (Socket_Link_Layer_Address : Socket_Link_Layer_Address_Type) return Boolean;
   
   function MAC_Address_Octets (Socket_Link_Layer_Address : Socket_Link_Layer_Address_Type) return Octets.Octets_Type;

private

   use type Interfaces.C.unsigned_char;
   use type Interfaces.C.unsigned_short;

   AF_LINK : constant := 18;

   type Socket_Link_Layer_Address_Type is
      record
         SDL_Len    : aliased Interfaces.C.unsigned_char;
         SDL_Family : aliased Interfaces.C.unsigned_char;
         SDL_Index  : aliased Interfaces.C.unsigned_short;
         SDL_Type   : aliased Interfaces.C.unsigned_char;
         SDL_Nlen   : aliased Interfaces.C.unsigned_char;
         SDL_Alen   : aliased Interfaces.C.unsigned_char;
         SDL_Slen   : aliased Interfaces.C.unsigned_char;
         SDL_Data   : aliased Octets.Octets_Type (1 .. 12);
      end record
     with
       Convention => C_Pass_By_Copy;

   function Is_Not_Link_Layer_Address (Socket_Link_Layer_Address : Socket_Link_Layer_Address_Type) return Boolean is
      (Socket_Link_Layer_Address.SDL_Family /= AF_LINK);

   function MAC_Address_Octets (Socket_Link_Layer_Address : Socket_Link_Layer_Address_Type) return Octets.Octets_Type is
      (Socket_Link_Layer_Address.SDL_Data ( Integer (Socket_Link_Layer_Address.SDL_Nlen + 1) .. Integer (Socket_Link_Layer_Address.SDL_Nlen + 6)));

end Packets.Pcap.Link_Layer_Addresses;
