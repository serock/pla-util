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

   use type Interfaces.C.unsigned_short;

   AF_PACKET : constant := 17;

   type Socket_Link_Layer_Address_Type is
      record
         SA_Family             : aliased Interfaces.C.unsigned_short;
         SLL_Protocol          : aliased Interfaces.C.unsigned_short;
         SLL_Interface_Index   : aliased Interfaces.C.int;
         SLL_ARP_Hardware_Kind : aliased Interfaces.C.unsigned_short;
         SLL_Packet_Kind       : aliased Interfaces.C.unsigned_char;
         SLL_Address_Length    : aliased Interfaces.C.unsigned_char;
         SLL_Address           : aliased Octets.Octets_Type (1 .. 8);
      end record
     with
       Convention => C_Pass_By_Copy;

   function Is_Not_Link_Layer_Address (Socket_Link_Layer_Address : Socket_Link_Layer_Address_Type) return Boolean is
      (Socket_Link_Layer_Address.SA_Family /= AF_PACKET);

   function MAC_Address_Octets (Socket_Link_Layer_Address : Socket_Link_Layer_Address_Type) return Octets.Octets_Type is
      (Socket_Link_Layer_Address.SLL_Address (1 .. 6));

end Packets.Pcap.Link_Layer_Addresses;
