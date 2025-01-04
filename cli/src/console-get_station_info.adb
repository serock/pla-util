--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016-2025 John Serock
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
with Firmware_Boot_Strings;

separate (Console)

procedure Get_Station_Info (Network_Device_Name : String;
                            PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is

   Column_2     : constant                                       := 34;
   Station_Info : constant Power_Line_Adapters.Station_Info_Type := Commands.Get_Station_Info (Network_Device_Name => Network_Device_Name,
                                                                                               PLA_MAC_Address     => PLA_MAC_Address);

   use type Power_Line_Adapters.Chip_Version_Type;
   use type Power_Line_Adapters.Firmware_Name_Type;

begin

   Ada.Text_IO.Put (Item => "Chip Version:");
   Ada.Text_IO.Set_Col (To => Column_2);
   Ada.Text_IO.Put (Item => Power_Line_Adapters.Chip_Version_Type'Image (Station_Info.Chip_Version));
   if Station_Info.Chip_Version = Power_Line_Adapters.Unknown then
      Ada.Text_IO.New_Line (Spacing => 1);
   else
      Ada.Text_IO.Put_Line (" (Full Id: " & Image (Chip_Id => Station_Info.Chip_Id) & ")");
   end if;
   Ada.Text_IO.Put (Item => "Hardware Version:");
   Ada.Text_IO.Set_Col (To => Column_2);
   Ada.Text_IO.Put_Line (Item => Image (Hardware_Version => Station_Info.Hardware_Version));
   if Station_Info.Chip_Version = Power_Line_Adapters.Unknown then
      return;
   end if;
   Ada.Text_IO.Put (Item => "Firmware Version:");
   Ada.Text_IO.Set_Col (To => Column_2);
   if Station_Info.Firmware_Version.Name = Power_Line_Adapters.INVALID then
      Ada.Text_IO.Put_Line (Item => -(+"%u.%u.%u (svn: %u)" &
                                    Natural (Station_Info.Firmware_Version.Major) & Natural (Station_Info.Firmware_Version.Minor) & Natural (Station_Info.Firmware_Version.Build) &
                                    Natural (Station_Info.Firmware_Revision)));
   else
      Ada.Text_IO.Put_Line (Item => -(+"%s %u.%u.%u (svn: %u)" &
                                    Station_Info.Firmware_Version.Name'Image &
                                    Natural (Station_Info.Firmware_Version.Major) & Natural (Station_Info.Firmware_Version.Minor) & Natural (Station_Info.Firmware_Version.Build) &
                                    Natural (Station_Info.Firmware_Revision)));
   end if;
   Ada.Text_IO.Put (Item => "ROM Version:");
   Ada.Text_IO.Set_Col (To => Column_2);
   Partial_Version_Text_IO.Put (Item  => Station_Info.ROM_Version.Major,
                                Width => 1,
                                Base  => 10);
   Ada.Text_IO.Put (Item => '.');
   Partial_Version_Text_IO.Put (Item  => Station_Info.ROM_Version.Minor,
                                Width => 1,
                                Base  => 10);
   Ada.Text_IO.Put (Item => '.');
   Partial_Version_Text_IO.Put (Item  => Station_Info.ROM_Version.Build,
                                Width => 1,
                                Base  => 10);
   Ada.Text_IO.New_Line (Spacing => 1);
   Ada.Text_IO.Put (Item => "Param Config Version (built-in):");
   Ada.Text_IO.Set_Col (To => Column_2);
   Param_Config_Version_Text_IO.Put (Item  => Station_Info.Param_Config_Built_In_Version,
                                     Width => 1,
                                     Base  => 10);
   Ada.Text_IO.New_Line (Spacing => 1);
   Ada.Text_IO.Put (Item => "Param Config Version (NVM):");
   Ada.Text_IO.Set_Col (To => Column_2);
   Param_Config_Version_Text_IO.Put (Item  => Station_Info.Param_Config_NVM_Version,
                                     Width => 1,
                                     Base  => 10);
   Ada.Text_IO.New_Line (Spacing => 1);
   Ada.Text_IO.Put (Item => "Flash Model:");
   Ada.Text_IO.Set_Col (To => Column_2);
   Ada.Text_IO.Put_Line (Item => Station_Info.Flash_Model'Image);
   Ada.Text_IO.Put (Item => "HomePlug Version:");
   Ada.Text_IO.Set_Col (To => Column_2);
   case Station_Info.Homeplug_Version is
      when Power_Line_Adapters.V_1_1  => Ada.Text_IO.Put_Line (Item => "1.1");
      when Power_Line_Adapters.V_2_0  => Ada.Text_IO.Put_Line (Item => "2.0");
      when others                     => Ada.Text_IO.Put_Line (Item => "Unknown");
   end case;
   Ada.Text_IO.Put (Item => "Maximum Bit Rate:");
   Ada.Text_IO.Set_Col (To => Column_2);
   case Station_Info.Max_Bit_Rate is
      when Power_Line_Adapters.MBR_200  => Ada.Text_IO.Put_Line (Item => "200 Mbps");
      when Power_Line_Adapters.MBR_1000 => Ada.Text_IO.Put_Line (Item => "1000 Mbps");
      when Power_Line_Adapters.MBR_1800 => Ada.Text_IO.Put_Line (Item => "1800 Mbps");
      when others                       => Ada.Text_IO.Put_Line (Item => "Unknown");
   end case;
   Ada.Text_IO.Put (Item => "Uptime:");
   Ada.Text_IO.Set_Col (To => Column_2);
   Ada.Text_IO.Put_Line (Item => Image (Station_Info.Uptime));
   Ada.Text_IO.Put_Line (Item => "Firmware Boot Message:");
   Ada.Text_IO.Put_Line (Item => Firmware_Boot_Strings.To_String (Source => Station_Info.Firmware_Boot_Message));

end Get_Station_Info;
