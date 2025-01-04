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
separate (Console)

procedure Get_Network_Stats (Network_Device_Name : String;
                             PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is

   Column_2           : constant                                             := 30;
   Network_Stats_List : constant Power_Line_Adapters.Network_Stats_List_Type := Commands.Get_Network_Stats (Network_Device_Name => Network_Device_Name,
                                                                                                            PLA_MAC_Address     => PLA_MAC_Address);

begin

   Ada.Text_IO.Put_Line (Item => "Number of stations:" & Integer'Image (Network_Stats_List'Length));

   for I in 1 .. Network_Stats_List'Length loop
      Ada.Text_IO.Put_Line (Item => "Station" & Integer'Image (I) & ":");
      Ada.Text_IO.Put (Item => "  Destination Address (DA):");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Network_Stats_List (I).Destination_Address.Image);
      Ada.Text_IO.Put (Item => "  Avg PHY Data Rate to DA:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Data_Rate_Text_IO.Put (Item  => Network_Stats_List (I).Average_Rate_To_Dest,
                             Width => 3,
                             Base  => 10);
      Ada.Text_IO.Put_Line (Item => " Mbps");
      Ada.Text_IO.Put (Item => "  Avg PHY Data Rate from DA:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Data_Rate_Text_IO.Put (Item  => Network_Stats_List (I).Average_Rate_From_Dest,
                             Width => 3,
                             Base  => 10);
      Ada.Text_IO.Put_Line (Item => " Mbps");
   end loop;

end Get_Network_Stats;
