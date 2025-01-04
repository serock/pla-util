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

procedure Get_Network_Info (Network_Device_Name : String;
                            Network_Scope       : Commands.Network_Scope_Type;
                            PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is

   Column_2          : constant                                            := 36;
   Network_Info      : Power_Line_Adapters.Network_Info_Type;
   Network_Info_List : constant Power_Line_Adapters.Network_Info_List_Type := Commands.Get_Network_Info (Network_Device_Name => Network_Device_Name,
                                                                                                         Network_Scope       => Network_Scope,
                                                                                                         PLA_MAC_Address     => PLA_MAC_Address);

begin

   Ada.Text_IO.Put_Line (Item => "Number of networks:" & Integer'Image (Network_Info_List'Length));

   for I in 1 .. Network_Info_List'Length loop
      Network_Info := Network_Info_List (I);
      Ada.Text_IO.Put_Line (Item => "Network" & Integer'Image (I) & ":");
      Ada.Text_IO.Put (Item => "  NID:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Image (NID => Network_Info.NID));
      Ada.Text_IO.Put (Item => "  SNID:");
      Ada.Text_IO.Set_Col (To => Column_2);
      SNID_Text_IO.Put (Item  => Network_Info.SNID,
                        Width => 1,
                        Base  => 10);
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put (Item => "  TEI:");
      Ada.Text_IO.Set_Col (To => Column_2);
      TEI_Text_IO.Put (Item  => Network_Info.TEI,
                       Width => 1,
                       Base  => 10);
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put (Item => "  CCo MAC Address:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Network_Info.CCo_MAC_Address.Image);
      Ada.Text_IO.Put (Item => "  Backup CCo MAC Address:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Network_Info.BCCo_MAC_Address.Image);
      Ada.Text_IO.Put (Item => "  Number of Coordinating Networks:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Network_Count_Text_IO.Put (Item  => Network_Info.Num_Coord_Networks,
                                 Width => 1,
                                 Base  => 10);
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put (Item => "  Station Role:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Power_Line_Adapters.Station_Role_Type'Image (Network_Info_List (I).Station_Role));
      Ada.Text_IO.Put (Item => "  Network Kind:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Power_Line_Adapters.Network_Kind_Type'Image (Network_Info_List (I).Network_Kind));
      Ada.Text_IO.Put (Item => "  Status:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Power_Line_Adapters.Status_Type'Image (Network_Info_List (I).Status));
      Ada.Text_IO.New_Line (Spacing => 1);
   end loop;

end Get_Network_Info;
