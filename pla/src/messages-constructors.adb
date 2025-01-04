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
package body Messages.Constructors is

   subtype Default_Request_Payload_Type  is Packets.Payload_Type (1 .. Packets.Minimum_Payload_Length);
   subtype Set_HFID_Request_Payload_Type is Packets.Payload_Type (1 .. 78);

   function Create_Check_DAK_1_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#09#, others => 16#00#));

      return Message;

   end Create_Check_DAK_1_Request;

   function Create_Check_DAK_2_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#03#, 16#0a#, others => 16#00#));

      return Message;

   end Create_Check_DAK_2_Request;

   function Create_Check_DAK_3_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#04#, 16#0b#, others => 16#00#));

      return Message;

   end Create_Check_DAK_3_Request;

   function Create_Check_DAK_4_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#05#, 16#0c#, others => 16#00#));

      return Message;

   end Create_Check_DAK_4_Request;

   function Create_Check_NMK_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#24#, others => 16#00#));

      return Message;

   end Create_Check_NMK_Request;

   function Create_Discover_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#01#, 16#70#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#,
                          16#a3#, 16#97#, 16#a2#, 16#55#, 16#53#, 16#be#, 16#f1#, 16#fc#, 16#f9#, 16#79#, 16#6b#, 16#52#, 16#14#, 16#13#, 16#e9#, 16#e2#,
                          others => 16#00#));

      return Message;

   end Create_Discover_Request;

   function Create_Get_Any_Network_Info_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#28#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#00#, 16#01#, others => 16#00#));

      return Message;

   end Create_Get_Any_Network_Info_Request;

   function Create_Get_Capabilities_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#01#, 16#34#, 16#60#, others => 16#00#));

      return Message;

   end Create_Get_Capabilities_Request;

   function Create_Get_Discover_List_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#01#, 16#14#, others => 16#00#));

      return Message;

   end Create_Get_Discover_List_Request;

   function Create_Get_Id_Info_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#01#, 16#60#, 16#60#, others => 16#00#));

      return Message;

   end Create_Get_Id_Info_Request;

   function Create_Get_Manufacturer_HFID_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#1b#, others => 16#00#));

      return Message;

   end Create_Get_Manufacturer_HFID_Request;

   function Create_Get_Member_Network_Info_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#28#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, others => 16#00#));

      return Message;

   end Create_Get_Member_Network_Info_Request;

   function Create_Get_Network_Stats_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#2c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#00#,
                          16#b0#, 16#f2#, 16#e6#, 16#95#, 16#66#, 16#6b#, 16#03#,
                          others => 16#00#));

      return Message;

   end Create_Get_Network_Stats_Request;

   function Create_Get_Station_Info_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#4c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, others => 16#00#));

      return Message;

   end Create_Get_Station_Info_Request;

   function Create_Get_User_HFID_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#25#, others => 16#00#));

      return Message;

   end Create_Get_User_HFID_Request;

   function Create_Reset_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#54#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#01#, others => 16#00#));

      return Message;

   end Create_Reset_Request;

   function Create_Restart_Request return Message_Type is

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          16#02#, 16#20#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, others => 16#00#));

      return Message;

   end Create_Restart_Request;

   function Create_Set_HFID_Request (HFID : HFID_Strings.Bounded_String) return Message_Type is

      use type Octets.Octets_Type;

      Message : Message_Type (Payload_Length => Set_HFID_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Set_HFID_Request_Payload_Type'(
                          (16#02#, 16#58#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#25#, 16#00#, 16#01#, 16#40#, 16#00#) &
                            Get_Octets (HFID => HFID)));

      return Message;

   end Create_Set_HFID_Request;

   function Create_Set_NMK_Request (Key : Octets.Key_Type) return Message_Type is

      use type Octets.Octets_Type;

      Message : Message_Type (Payload_Length => Default_Request_Payload_Type'Length);

   begin

      Message.Initialize (Message_Payload => Default_Request_Payload_Type'(
                          (16#02#, 16#18#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#) &
                            Key &
                          (16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#)));

      return Message;

   end Create_Set_NMK_Request;

   function Get_Octets (HFID : HFID_Strings.Bounded_String) return Octets.HFID_Octets_Type is

      HFID_Octets : Octets.HFID_Octets_Type            := (others => 16#00#);
      Length      : constant HFID_Strings.Length_Range := HFID_Strings.Length (Source => HFID);

   begin

      for I in 1 .. Length loop
         HFID_Octets (I) := Character'Pos (HFID_Strings.Element (Source => HFID,
                                                                 Index  => I));
      end loop;

      return HFID_Octets;

   end Get_Octets;

end Messages.Constructors;
