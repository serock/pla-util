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
with Packets.Network_Devices;

package body Power_Line_Adapters.Network_Device is

   Network_Device : Packets.Network_Devices.Network_Device_Type;

   function Derive_Protocol (Payload : Packets.Payload_Type) return Packets.Protocol_Type is

      use type Octets.Octet_Type;

   begin

      return (if Payload (3) = 16#a0# then Power_Line_Adapters.Protocol_Mediaxtream else Power_Line_Adapters.Protocol_Homeplug);

   end Derive_Protocol;

   procedure Open (Network_Device_Name : String) is
   begin

      Network_Device.Open (Network_Device_Name => Network_Device_Name);

   end Open;

   procedure Send (Payload     : Packets.Payload_Type;
                   Destination : MAC_Addresses.MAC_Address_Type) is
   begin

      Network_Device.Send (Payload     => Payload,
                           Protocol    => Derive_Protocol (Payload => Payload),
                           Destination => Destination);

   end Send;

end Power_Line_Adapters.Network_Device;
