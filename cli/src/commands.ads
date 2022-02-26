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
with HFID_Strings;
with MAC_Addresses;
with Power_Line_Adapter_Sets;
with Power_Line_Adapters;

package Commands is

   type Command_Type is (Check_DAK,
                         Check_NMK,
                         Discover,
                         Get_Capabilities,
                         Get_Discover_List,
                         Get_HFID,
                         Get_Id_Info,
                         Get_Network_Info,
                         Get_Network_Stats,
                         Reset,
                         Restart,
                         Set_HFID,
                         Set_NMK);

   type HFID_Level_Type is (Manufacturer,
                            User);

   type Network_Scope_Type is (Member,
                               Any);

   Command_Error : exception;

   function Check_DAK (Network_Device_Name : String;
                       Pass_Phrase         : String;
                       PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) return Boolean;

   function Check_NMK (Network_Device_Name : String;
                       Pass_Phrase         : String;
                       PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) return Boolean;

   function Discover (Network_Device_Name : String) return Power_Line_Adapter_Sets.Set;

   function Get_Capabilities (Network_Device_Name : String;
                              PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) return Power_Line_Adapters.Capabilities_Type;

   function Get_Discover_List (Network_Device_Name : String;
                               PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) return Power_Line_Adapters.Discover_List_Type;

   function Get_HFID (Network_Device_Name : String;
                      HFID_Level          : HFID_Level_Type;
                      PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) return HFID_Strings.Bounded_String;

   function Get_Id_Info (Network_Device_Name : String;
                         PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) return Power_Line_Adapters.Id_Info_Type;

   function Get_Network_Info (Network_Device_Name : String;
                              Network_Scope       : Network_Scope_Type;
                              PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) return Power_Line_Adapters.Network_Info_List_Type;

   function Get_Network_Stats (Network_Device_Name : String;
                               PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) return Power_Line_Adapters.Network_Stats_List_Type;

   procedure Reset (Network_Device_Name : String;
                    PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type);

   procedure Restart (Network_Device_Name : String;
                      PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type);

   procedure Set_HFID (Network_Device_Name : String;
                       HFID                : HFID_Strings.Bounded_String;
                       PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type);

   procedure Set_NMK (Network_Device_Name : String;
                      Pass_Phrase         : String;
                      PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type);

private

   No_MAC_Address : constant String := "00:00:00:00:00:00";

end Commands;
