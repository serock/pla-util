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
private with Interfaces.C;
private with Packets.Pcap;

package Packets.Network_Devices is

   type Network_Device_Type is tagged limited private;

   function Is_Available (Self : Network_Device_Type) return Boolean;

   function Name (Self : Network_Device_Type) return String;

   procedure Open (Self : in out Network_Device_Type;
                   Name :        String)
     with
       Pre => Self.Is_Available;

   procedure Open (Self : in out Network_Device_Type)
     with
       Pre => Self.Is_Available;

   procedure Send (Self        : Network_Device_Type;
                   Payload     : Payload_Type;
                   Protocol    : Protocol_Type;
                   Destination : MAC_Addresses.MAC_Address_Type);

private

   use type Pcap.Pcap_Access_Type;

   type Network_Device_Type is new Ada.Finalization.Limited_Controlled with
      record
         Interface_Name : Interfaces.C.char_array (1 .. 16) := (others => Interfaces.C.nul);
         Handle         : Pcap.Pcap_Access_Type             := null;
         MAC_Address    : MAC_Addresses.MAC_Address_Type;
      end record;

   overriding procedure Finalize (Self : in out Network_Device_Type)
     with
       Post => Self.Is_Available;

   function Is_Available (Self : Network_Device_Type) return Boolean is
     (Self.Handle = null);

end Packets.Network_Devices;
