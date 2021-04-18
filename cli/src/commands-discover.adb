------------------------------------------------------------------------
--  pla-util - A powerline adapter utility
--  Copyright (C) 2016-2021 John Serock
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
with Packet_Sockets.Thin;
with Power_Line_Adapter.Network;
with Power_Line_Adapter_Sets;

separate (Commands)

function Discover (Device_Name : in String) return Power_Line_Adapter_Sets.Set is

   Adapters : Power_Line_Adapter_Sets.Set (Capacity => Power_Line_Adapter.Max_Adapters);
   Socket   : Packet_Sockets.Thin.Socket_Type;

begin

   Socket.Open (Protocol        => Packet_Sockets.Thin.Protocol_8912,
                Device_Name     => Device_Name,
                Receive_Timeout => Default_Receive_Timeout,
                Send_Timeout    => Default_Send_Timeout);

   Adapters := Power_Line_Adapter.Network.Discover (Socket => Socket);

   Socket.Close;

   return Adapters;

exception

   when others =>

      Socket.Close;

      raise;

end Discover;
