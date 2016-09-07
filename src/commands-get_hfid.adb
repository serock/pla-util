------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016 John Serock
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
with Ethernet.Datagram_Socket;
with Power_Line_Adapter.Network;
with Power_Line_Adapter_Sets;

separate (Commands)

function Get_HFID(Device_Name : in String;
                  HFID_Level  : in HFID_Level_Type) return HFID_String.Bounded_String is

   Adapters : Power_Line_Adapter_Sets.Set(Capacity => Power_Line_Adapter.Max_Adapters);
   HFID     : HFID_String.Bounded_String;
   Socket   : Ethernet.Datagram_Socket.Socket_Type;

   use type Ada.Containers.Count_Type;

begin

   Socket.Open(Protocol        => Ethernet.Datagram_Socket.Protocol_8912,
               Device_Name     => Device_Name,
               Receive_Timeout => Default_Receive_Timeout,
               Send_Timeout    => Default_Send_Timeout);

   Adapters := Power_Line_Adapter.Network.Discover_Adapters(Socket => Socket);

   if Adapters.Length = 0 then

      raise Command_Exception with "No adapters were discovered";

   end if;

   case HFID_Level is

      when Manufacturer =>

         HFID := Adapters.First_Element.Get_Manufacturer_HFID(Socket => Socket);

      when User =>

         HFID := Adapters.First_Element.Get_User_HFID(Socket => Socket);

   end case;

   Socket.Close;

   return HFID;

exception

   when others =>

      Socket.Close;

      raise;

end Get_HFID;
