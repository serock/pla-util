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
private with Commands;
private with MAC_Addresses;
private with Power_Line_Adapters;

package Console is

   procedure Process_Command_Line;

private

   procedure Check_DAK (Network_Device_Name : String);
   procedure Check_NMK (Network_Device_Name : String);
   procedure Discover (Network_Device_Name : String);
   procedure Get_Capabilities (Network_Device_Name : String);
   procedure Get_Discover_List (Network_Device_Name : String);
   procedure Get_HFID (Network_Device_Name : String);
   function Get_HFID_Level return Commands.HFID_Level_Type;
   procedure Get_Id_Info (Network_Device_Name : String);
   procedure Get_Network_Info (Network_Device_Name : String);
   function Get_Network_Scope return Commands.Network_Scope_Type;
   procedure Get_Network_Stats (Network_Device_Name : String);
   function Get_PLA_MAC_Address (Image : String) return MAC_Addresses.MAC_Address_Type;
   function Image (HPAV_Version : Power_Line_Adapters.HPAV_Version_Type) return String;
   function Image (NID : Power_Line_Adapters.NID_Type) return String;
   function Image (Signal_Level : Power_Line_Adapters.Signal_Level_Type) return String;
   procedure Reset (Network_Device_Name : String);
   procedure Restart (Network_Device_Name : String);
   function Security_Level (NID : Power_Line_Adapters.NID_Type) return String;
   procedure Set_HFID (Network_Device_Name : String);
   procedure Set_NMK (Network_Device_Name : String);
   procedure Show_Help;
   procedure Show_Version;
   function To_Command (Source : String) return Commands.Command_Type;

end Console;
