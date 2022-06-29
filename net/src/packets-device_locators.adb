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
with Interfaces.C.Strings;

package body Packets.Device_Locators is

   function Create return Basic_Search_Type is

      Algorithm : Basic_Search_Type;

   begin

      return Algorithm;

   end Create;

   function Create (Device_Name : String) return Named_Search_Type is

      Algorithm    : Named_Search_Type;
      Target_Count : Interfaces.C.size_t;

   begin

      Interfaces.C.To_C (Item   => Device_Name,
                         Target => Algorithm.Network_Device_Name,
                         Count  => Target_Count);

      return Algorithm;

   end Create;

   overriding procedure Finalize (Self : in out Device_Locator_Type) is
   begin

      if Self.Devices_Access /= null then

         Pcap.Devices.Free_All_Devices (Network_Devices => Self.Devices_Access);

         Self.Devices_Access := null;

      end if;

   end Finalize;

   procedure Find (Self              : in out Device_Locator_Type;
                   Device_Name       :        String;
                   Interface_Name    :    out Interface_Name_Strings.Bounded_String;
                   Interface_Address :    out MAC_Addresses.MAC_Address_Type) is

      use type Interfaces.C.int;
      use type Pcap.Devices.Address_Access_Type;

      Address_Access   : Pcap.Devices.Address_Access_Type   := null;
      Device_Access    : Pcap.Devices.Interface_Access_Type := null;
      Error_Buffer     : aliased Pcap.Error_Buffer_Type;
      Return_Code      : Interfaces.C.int;
      Search_Algorithm : Search_Type'Class                  := (if Device_Name = "" then Create else Create (Device_Name => Device_Name));

   begin

      Return_Code := Pcap.Devices.Find_All_Devices (Network_Devices => Self.Devices_Access,
                                                    Error_Buffer    => Error_Buffer);

      if Return_Code /= 0 then
         raise Packet_Error with Interfaces.C.To_Ada (Item => Error_Buffer);
      end if;

      if Self.Devices_Access = null then
         raise Packet_Error with "No devices found";
      end if;

      Device_Access  := Search_Algorithm.Run (Network_Device => Self.Devices_Access);
      Interface_Name := Interface_Name_Strings.To_Bounded_String (Source => Interfaces.C.Strings.Value (Item => Device_Access.all.Name));
      Address_Access := Device_Access.all.Addresses;

      while Address_Access /= null and then Pcap.Devices.Is_Not_Packet_Address (Socket_Address => Address_Access.all.Socket_Address) loop
         Address_Access := Address_Access.all.Next;
      end loop;

      if Address_Access = null then
         raise Packet_Error with "MAC address of device " & Interface_Name_Strings.To_String (Source => Interface_Name) & " not found";
      end if;

      Interface_Address := MAC_Addresses.Create_MAC_Address (MAC_Address_Octets => Address_Access.all.Socket_Address.all.SLL_Address (1 .. 6));

   end Find;

   overriding function Run (Self           : Basic_Search_Type;
                            Network_Device : Pcap.Devices.Interface_Access_Type) return Pcap.Devices.Interface_Access_Type is

      Device_Access : Pcap.Devices.Interface_Access_Type := Network_Device;

   begin

      while Device_Access /= null and then
        (Pcap.Devices.Is_Loopback (Network_Device => Device_Access) or else
         Pcap.Devices.Is_Down (Network_Device => Device_Access) or else
         Pcap.Devices.Is_Not_Running (Network_Device => Device_Access)) loop

         Device_Access := Device_Access.all.Next;

      end loop;

      if Device_Access = null then
         raise Packet_Error with "No up and running non-loopback network device found";
      end if;

      return Device_Access;

   end Run;

   overriding function Run (Self           : Named_Search_Type;
                            Network_Device : Pcap.Devices.Interface_Access_Type) return Pcap.Devices.Interface_Access_Type is

      Device_Access : Pcap.Devices.Interface_Access_Type := Network_Device;

   begin

      while Device_Access /= null and then Interfaces.C.Strings.Value (Item => Device_Access.all.Name) /= Interfaces.C.To_Ada (Item => Self.Network_Device_Name) loop
         Device_Access := Device_Access.all.Next;
      end loop;

      if Device_Access = null then
         raise Packet_Error with "Device " & Interfaces.C.To_Ada (Item => Self.Network_Device_Name) & " not found";
      end if;

      return Device_Access;

   end Run;

end Packets.Device_Locators;
