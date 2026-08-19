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
with Packets.Pcap.Link_Layer_Addresses;

package Packets.Pcap.Devices is

   type Interface_Type;
   type Interface_Access_Type is access Interface_Type;

   type Address_Type;
   type Address_Access_Type is access Address_Type;

   type Interface_Type is
      record
         Next        : Interface_Access_Type;
         Name        : Interfaces.C.Strings.chars_ptr;
         Description : Interfaces.C.Strings.chars_ptr;
         Addresses   : Address_Access_Type;
         Flags       : aliased Interfaces.C.unsigned;
      end record
     with
       Convention => C_Pass_By_Copy;

   type Address_Type is
      record
         Next                : Address_Access_Type;
         Socket_Address      : Packets.Pcap.Link_Layer_Addresses.Socket_Link_Layer_Address_Access_Type;
         Netmask             : Packets.Pcap.Link_Layer_Addresses.Socket_Link_Layer_Address_Access_Type;
         Broadcast_Address   : Packets.Pcap.Link_Layer_Addresses.Socket_Link_Layer_Address_Access_Type;
         Destination_Address : Packets.Pcap.Link_Layer_Addresses.Socket_Link_Layer_Address_Access_Type;
      end record
     with
       Convention => C_Pass_By_Copy;

   function Find_All_Devices (Network_Devices : out Interface_Access_Type;
                              Error_Buffer    : out Error_Buffer_Type) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_findalldevs";

   procedure Free_All_Devices (Network_Devices : Interface_Access_Type)
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_freealldevs";

   function Is_Down (Network_Device : Interface_Access_Type) return Boolean;

   function Is_Loopback (Network_Device : Interface_Access_Type) return Boolean;

   function Is_Not_Running (Network_Device : Interface_Access_Type) return Boolean;

private

   use type Interfaces.C.unsigned;

   IF_LOOPBACK : constant Interfaces.C.unsigned := 16#0000_0001#;
   IF_UP       : constant Interfaces.C.unsigned := 16#0000_0002#;
   IF_RUNNING  : constant Interfaces.C.unsigned := 16#0000_0004#;

   function Is_Down (Network_Device : Interface_Access_Type) return Boolean is
     ((Network_Device.all.Flags and IF_UP) = 0);

   function Is_Loopback (Network_Device : Interface_Access_Type) return Boolean is
     ((Network_Device.all.Flags and IF_LOOPBACK) /= 0);

   function Is_Not_Running (Network_Device : Interface_Access_Type) return Boolean is
     ((Network_Device.all.Flags and IF_RUNNING) = 0);

end Packets.Pcap.Devices;
