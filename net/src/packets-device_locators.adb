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
with Interfaces.C.Strings;

package body Packets.Device_Locators is

   overriding procedure Finalize (Self : in out Device_Locator_Type) is
   begin

      if Self.Devices_Access /= null then

         Pcap.Devices.Free_All_Devices (Devices => Self.Devices_Access);

         Self.Devices_Access := null;

      end if;

   end Finalize;

   function Find (Self        : in out Device_Locator_Type;
                  Device_Name :        String) return MAC_Addresses.MAC_Address_Type is

      use type Interfaces.C.int;
      use type Interfaces.C.unsigned_short;
      use type Pcap.Devices.Address_Access_Type;

      Address_Access : Pcap.Devices.Address_Access_Type   := null;
      Device         : Pcap.Devices.Interface_Access_Type := null;
      Error_Buffer   : aliased Pcap.Error_Buffer_Type;
      Return_Code    : Interfaces.C.int;

   begin

      Return_Code := Pcap.Devices.Find_All_Devices (Devices      => Self.Devices_Access,
                                                    Error_Buffer => Error_Buffer);

      if Return_Code /= 0 then
         raise Packet_Error with Interfaces.C.To_Ada (Item => Error_Buffer);
      end if;

      if Self.Devices_Access = null then
         raise Packet_Error with "No devices found";
      end if;

      Device := Self.Devices_Access;

      while Device /= null and then Interfaces.C.Strings.Value (Item => Device.Name) /= Device_Name loop
         Device := Device.Next;
      end loop;

      if Device = null then
         raise Packet_Error with "Device " & Device_Name & " not found";
      end if;

      Address_Access := Device.Addresses;

      while Address_Access /= null and then Address_Access.Addr.SA_Family /= Pcap.Devices.AF_PACKET loop
         Address_Access := Address_Access.Next;
      end loop;

      if Address_Access = null then
         raise Packet_Error with "MAC address of device " & Device_Name & " not found";
      end if;

      return MAC_Addresses.Create_MAC_Address (Octets => Address_Access.Addr.SLL_Address (1 .. 6));

   end Find;

end Packets.Device_Locators;
