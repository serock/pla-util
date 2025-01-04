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

procedure Get_Capabilities (Network_Device_Name : String;
                            PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is

   Column_2     : constant                                       := 25;
   Capabilities : constant Power_Line_Adapters.Capabilities_Type := Commands.Get_Capabilities (Network_Device_Name => Network_Device_Name,
                                                                                               PLA_MAC_Address     => PLA_MAC_Address);

begin

   Ada.Text_IO.Put (Item => "AV Version:");
   Ada.Text_IO.Set_Col (To => Column_2);
   Ada.Text_IO.Put_Line (Item => Image (HPAV_Version => Capabilities.AV_Version));
   Ada.Text_IO.Put (Item => "MAC Address:");
   Ada.Text_IO.Set_Col (To => Column_2);
   Ada.Text_IO.Put_Line (Item => Capabilities.MAC_Address.Image);
   Ada.Text_IO.Put (Item => "OUI:");
   Ada.Text_IO.Set_Col (To => Column_2);
   Ada.Text_IO.Put (Item => -(+"%06x" & Natural (Capabilities.OUI)));
   Ada.Text_IO.New_Line (Spacing => 1);
   Ada.Text_IO.Put (Item => "Backup CCo:");
   Ada.Text_IO.Set_Col (To => Column_2);
   Ada.Text_IO.Put_Line (Item => Capabilities.Backup_CCo'Image);
   Ada.Text_IO.Put (Item => "Proxy:");
   Ada.Text_IO.Set_Col (To => Column_2);
   Ada.Text_IO.Put_Line (Item => Capabilities.Proxy'Image);
   Ada.Text_IO.Put (Item => "Implementation Version:");
   Ada.Text_IO.Set_Col (To => Column_2 - 1);
   Ada.Text_IO.Put_Line (Item => Capabilities.Implementation_Version'Image);

end Get_Capabilities;
