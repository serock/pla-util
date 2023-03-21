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
with Interfaces.C;
with MAC_Addresses;
with Packets.Pcap.Devices;
pragma Elaborate (Packets.Pcap.Devices);

private with Ada.Finalization;

private package Packets.Device_Locators is

   type Search_Type is interface;

   function Run (Self           : Search_Type;
                 Network_Device : Pcap.Devices.Interface_Access_Type) return Pcap.Devices.Interface_Access_Type is abstract;

   type Basic_Search_Type is new Search_Type with null record;

   overriding function Run (Self           : Basic_Search_Type;
                            Network_Device : Pcap.Devices.Interface_Access_Type) return Pcap.Devices.Interface_Access_Type;

   function Create return Basic_Search_Type;

   type Named_Search_Type is new Search_Type with
      record
         Network_Device_Name : Interfaces.C.char_array (1 .. 16) := (others => Interfaces.C.nul);
      end record;

   overriding function Run (Self           : Named_Search_Type;
                            Network_Device : Pcap.Devices.Interface_Access_Type) return Pcap.Devices.Interface_Access_Type;

   function Create (Device_Name : String) return Named_Search_Type;

   type Device_Locator_Type is tagged limited private;

   procedure Find (Self              :     Device_Locator_Type;
                   Device_Name       :     String;
                   Interface_Name    : out Interface_Name_Strings.Bounded_String;
                   Interface_Address : out MAC_Addresses.MAC_Address_Type);

   procedure Find_All_Devices (Self : in out Device_Locator_Type)
     with
       Pre => Self.Is_Available;

   function Is_Available (Self : Device_Locator_Type) return Boolean;

private

   use type Pcap.Devices.Interface_Access_Type;

   type Device_Locator_Type is new Ada.Finalization.Limited_Controlled with
      record
         Devices_Access : Pcap.Devices.Interface_Access_Type := null;
      end record;

   overriding procedure Finalize (Self : in out Device_Locator_Type)
     with
       Post => Self.Is_Available;

   function Is_Available (Self : Device_Locator_Type) return Boolean is
     (Self.Devices_Access = null);

end Packets.Device_Locators;
