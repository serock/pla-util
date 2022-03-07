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
with MAC_Addresses;

private with Ada.Finalization;
private with Packets.Pcap.Devices;

package Packets.Device_Locators is

   type Device_Locator_Type is tagged limited private;

   function Find (Self        : in out Device_Locator_Type;
                  Device_Name :        String) return MAC_Addresses.MAC_Address_Type
     with
       Pre => Self.Is_Available;

   function Is_Available (Self : in out Device_Locator_Type) return Boolean;

private

   use type Pcap.Devices.Interface_Access_Type;

   type Device_Locator_Type is new Ada.Finalization.Limited_Controlled with
      record

         Devices_Access : Pcap.Devices.Interface_Access_Type := null;

      end record;

   overriding procedure Finalize (Self : in out Device_Locator_Type)
     with
       Post => Self.Is_Available;

   function Is_Available (Self : in out Device_Locator_Type) return Boolean is
     (Self.Devices_Access = null);

end Packets.Device_Locators;
