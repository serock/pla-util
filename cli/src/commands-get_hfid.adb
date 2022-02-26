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
with Ada.Containers;
with MAC_Addresses;
with Power_Line_Adapter_Sets;
with Power_Line_Adapters.Network;

use type Ada.Containers.Count_Type;
use type MAC_Addresses.MAC_Address_Type;

separate (Commands)

function Get_HFID (Network_Device_Name : String;
                   HFID_Level          : HFID_Level_Type;
                   PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) return HFID_Strings.Bounded_String is

   Adapters : constant Power_Line_Adapter_Sets.Set := Power_Line_Adapters.Network.Discover (Network_Device_Name => Network_Device_Name,
                                                                                            MAC_Address         => PLA_MAC_Address);

begin

   if Adapters.Length = 0 then
      raise Command_Error with (if PLA_MAC_Address = MAC_Addresses.Broadcast_MAC_Address then Message_No_Adapters else Message_Not_Found);
   end if;

   case HFID_Level is

      when Manufacturer =>

         return Adapters.First_Element.Get_Manufacturer_HFID (Network_Device_Name => Network_Device_Name);

      when User =>

         return Adapters.First_Element.Get_User_HFID (Network_Device_Name => Network_Device_Name);

   end case;

end Get_HFID;
