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
with Ada.Containers;
with Packet_Sockets.Thin;
with Power_Line_Adapter.Network;
with Power_Line_Adapter_Sets;

use type Ada.Containers.Count_Type;

separate (Commands)

function Check_NMK (Device_Name : String;
                    Pass_Phrase : String) return Boolean is

   Adapters : Power_Line_Adapter_Sets.Set (Capacity => Power_Line_Adapter.Max_Adapters);
   Result   : Boolean;
   Socket   : Packet_Sockets.Thin.Socket_Type;

begin

   Socket.Open (Protocol        => Packet_Sockets.Thin.Protocol_8912,
                Device_Name     => Device_Name,
                Receive_Timeout => Default_Receive_Timeout,
                Send_Timeout    => Default_Send_Timeout);

   Adapters := Power_Line_Adapter.Network.Discover (Socket => Socket);

   if Adapters.Length = 0 then
      raise Command_Error with Message_No_Adapters;
   end if;

   Result := Adapters.First_Element.Check_NMK (Pass_Phrase => Pass_Phrase,
                                               Socket      => Socket);

   Socket.Close;

   return Result;

exception

   when others =>
      Socket.Close;
      raise;

end Check_NMK;
