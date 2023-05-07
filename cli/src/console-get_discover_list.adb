--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016-2023 John Serock
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
separate (Console)

procedure Get_Discover_List (Network_Device_Name : String;
                             PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is

   Column_2      : constant                                        := 24;
   Discover_List : constant Power_Line_Adapters.Discover_List_Type := Commands.Get_Discover_List (Network_Device_Name => Network_Device_Name,
                                                                                                  PLA_MAC_Address     => PLA_MAC_Address);

begin

   Ada.Text_IO.Put_Line (Item => "Number of stations:" & Discover_List.Number_Of_Stations'Image);

   for I in 1 .. Discover_List.Stations'Length loop
      Ada.Text_IO.Put_Line (Item => "Station" & Integer'Image (I) & ":");
      Ada.Text_IO.Put (Item => "  MAC Address:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Discover_List.Stations (I).MAC_Address.Image);
      Ada.Text_IO.Put (Item => "  TEI:");
      Ada.Text_IO.Set_Col (To => Column_2);
      TEI_Text_IO.Put (Item  => Discover_List.Stations (I).TEI,
                       Width => 1,
                       Base  => 10);
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put (Item => "  Same Network:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item  => Discover_List.Stations (I).Same_Network'Image);
      Ada.Text_IO.Put (Item => "  SNID:");
      Ada.Text_IO.Set_Col (To => Column_2);
      SNID_Text_IO.Put (Item  => Discover_List.Stations (I).SNID,
                        Width => 1,
                        Base  => 10);
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put (Item => "  CCo:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Discover_List.Stations (I).CCo'Image);
      Ada.Text_IO.Put (Item => "  PCo:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Discover_List.Stations (I).PCo'Image);
      Ada.Text_IO.Put (Item => "  Backup CCo:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Discover_List.Stations (I).Backup_CCo'Image);
      Ada.Text_IO.Put (Item => "  Signal Level:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Image (Signal_Level => Discover_List.Stations (I).Signal_Level));
   end loop;

   Ada.Text_IO.Put_Line (Item => "Number of Networks:" & Discover_List.Number_Of_Networks'Image);

   for I in 1 .. Discover_List.Networks'Length loop
      Ada.Text_IO.Put_Line (Item => "Network" & Integer'Image (I) & ":");
      Ada.Text_IO.Put (Item => "  NID:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Image (NID => Discover_List.Networks (I).NID));
      Ada.Text_IO.Put (Item => "  SNID:");
      Ada.Text_IO.Set_Col (To => Column_2);
      SNID_Text_IO.Put (Item  => Discover_List.Networks (I).SNID,
                        Width => 1,
                        Base  => 10);
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put (Item => "  Coordinating Status:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item  => Discover_List.Networks (I).Coordinating_Status'Image);
   end loop;

end Get_Discover_List;
