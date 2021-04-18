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

procedure Reset (Device_Name     : in String;
                 PLA_MAC_Address : in String) is

   Adapters : Power_Line_Adapter_Sets.Set (Capacity => Power_Line_Adapter.Max_Adapters);
   Socket   : Packet_Sockets.Thin.Socket_Type;
   Found    : Boolean := False;

begin

   Socket.Open (Protocol        => Packet_Sockets.Thin.Protocol_8912,
                Device_Name     => Device_Name,
                Receive_Timeout => Default_Receive_Timeout,
                Send_Timeout    => Default_Send_Timeout);

   Adapters := Power_Line_Adapter.Network.Discover (Socket => Socket);

   if Adapters.Length = 0 then

      raise Command_Error with Message_No_Adapters;

   end if;

   for E of Adapters loop

      if Power_Line_Adapter.Has_MAC_Address (Adapter => E, MAC_Address => PLA_MAC_Address) then

         E.Reset (Socket => Socket);
         Found := True;
         exit;

      end if;

   end loop;

   if not Found then

      raise Command_Error with Message_Not_Found;

   end if;

   Socket.Close;

exception

   when others =>

      Socket.Close;

      raise;

end Reset;
