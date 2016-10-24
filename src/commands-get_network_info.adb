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

use type Ada.Containers.Count_Type;

separate (Commands)

function Get_Network_Info(Device_Name   : in String;
                          Network_Scope : in Network_Scope_Type) return Power_Line_Adapter.Network_Info_List_Type is

   Adapters : Power_Line_Adapter_Sets.Set(Capacity => Power_Line_Adapter.Max_Adapters);
   Socket   : Ethernet.Datagram_Socket.Socket_Type;

begin

   Socket.Open(Protocol        => Ethernet.Datagram_Socket.Protocol_8912,
               Device_Name     => Device_Name,
               Receive_Timeout => Default_Receive_Timeout,
               Send_Timeout    => Default_Send_Timeout);

   Adapters := Power_Line_Adapter.Network.Discover(Socket => Socket);

   if Adapters.Length = 0 then

      raise Command_Error with Message_No_Adapters;

   end if;

   case Network_Scope is

      when Member =>

         declare

            Network_Info_List : Power_Line_Adapter.Network_Info_List_Type := Adapters.First_Element.Get_Member_Network_Info(Socket => Socket);

         begin

            Socket.Close;

            return Network_Info_List;

         end;

      when Any =>

         declare

            Network_Info_List : Power_Line_Adapter.Network_Info_List_Type := Adapters.First_Element.Get_Any_Network_Info(Socket => Socket);

         begin

            Socket.Close;

            return Network_Info_List;

         end;

   end case;

exception

   when others =>

      Socket.Close;

      raise;

end Get_Network_Info;
