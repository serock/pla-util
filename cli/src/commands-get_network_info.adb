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
with Power_Line_Adapter.Network;
with Power_Line_Adapter_Sets;

use type Ada.Containers.Count_Type;

separate (Commands)

function Get_Network_Info (Network_Device_Name : String;
                           Network_Scope       : Network_Scope_Type) return Power_Line_Adapter.Network_Info_List_Type is

   Adapters : Power_Line_Adapter_Sets.Set (Capacity => Power_Line_Adapter.Max_Adapters);

begin

   Adapters := Power_Line_Adapter.Network.Discover (Network_Device_Name => Network_Device_Name);

   if Adapters.Length = 0 then
      raise Command_Error with Message_No_Adapters;
   end if;

   case Network_Scope is
      when Member =>

         declare

            Network_Info_List : constant Power_Line_Adapter.Network_Info_List_Type := Adapters.First_Element.Get_Member_Network_Info (Network_Device_Name => Network_Device_Name);

         begin

            return Network_Info_List;

         end;

      when Any =>

         declare

            Network_Info_List : constant Power_Line_Adapter.Network_Info_List_Type := Adapters.First_Element.Get_Any_Network_Info (Network_Device_Name => Network_Device_Name);

         begin

            return Network_Info_List;

         end;
   end case;

end Get_Network_Info;
