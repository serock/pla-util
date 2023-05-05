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
with HFID_Strings;
with Octets;

package Messages.Constructors is

   function Create_Check_DAK_1_Request return Message_Type;

   function Create_Check_DAK_2_Request return Message_Type;

   function Create_Check_DAK_3_Request return Message_Type;

   function Create_Check_DAK_4_Request return Message_Type;

   function Create_Check_NMK_Request return Message_Type;

   function Create_Discover_Request return Message_Type;

   function Create_Get_Any_Network_Info_Request return Message_Type;

   function Create_Get_Capabilities_Request return Message_Type;

   function Create_Get_Discover_List_Request return Message_Type;

   function Create_Get_Id_Info_Request return Message_Type;

   function Create_Get_Manufacturer_HFID_Request return Message_Type;

   function Create_Get_Member_Network_Info_Request return Message_Type;

   function Create_Get_Network_Stats_Request return Message_Type;

   function Create_Get_Station_Info_Request return Message_Type;

   function Create_Get_User_HFID_Request return Message_Type;

   function Create_Reset_Request return Message_Type;

   function Create_Restart_Request return Message_Type;

   function Create_Set_HFID_Request (HFID : HFID_Strings.Bounded_String) return Message_Type;

   function Create_Set_NMK_Request (Key : Octets.Key_Type) return Message_Type;

private

   function Get_Octets (HFID : HFID_Strings.Bounded_String) return Octets.HFID_Octets_Type;

end Messages.Constructors;
