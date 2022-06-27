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
with Messages;
with Packets;
with Power_Line_Adapter_Sets;

package Power_Line_Adapters.Network is

   function Discover (Network_Device_Name : String;
                      MAC_Address         : MAC_Addresses.MAC_Address_Type := MAC_Addresses.Broadcast_MAC_Address) return Power_Line_Adapter_Sets.Set;

   procedure Receive (Confirmation        : out Packets.Payload_Type;
                      Confirmation_Length : out Natural;
                      From_MAC_Address    : out MAC_Addresses.MAC_Address_Type);

   procedure Send (Message     : Messages.Message_Type;
                   Destination : MAC_Addresses.MAC_Address_Type);

private

   function Receive_Discovered_Adapter (Adapters : in out Power_Line_Adapter_Sets.Set) return Boolean;

end Power_Line_Adapters.Network;
