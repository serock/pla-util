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
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with GNAT.Formatted_String;
with HFID_String;
with Power_Line_Adapter;
with Power_Line_Adapter_Sets;

use GNAT.Formatted_String;
use type Ada.Text_IO.Count;
use type Power_Line_Adapter.NID_Type;

package body Console is

   package Data_Rate_Text_IO     is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapter.Data_Rate_Type);
   package Network_Count_Text_IO is new Ada.Text_IO.Integer_IO (Num => Power_Line_Adapter.Network_Count_Type);
   package NID_Text_IO           is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapter.NID_Type);
   package OUI_Text_IO           is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapter.OUI_Type);
   package SNID_Text_IO          is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapter.SNID_Type);
   package TEI_Text_IO           is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapter.TEI_Type);

   function NID_Format is new GNAT.Formatted_String.Mod_Format (Int => Power_Line_Adapter.NID_Type,
                                                                Put => NID_Text_IO.Put);

   function OUI_Format is new GNAT.Formatted_String.Mod_Format (Int => Power_Line_Adapter.OUI_Type,
                                                                Put => OUI_Text_IO.Put);

   Message_Too_Few_Arguments   : constant String                             := "Too few arguments";
   Separator_Character_Mapping : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping (From => "-",
                                                                                                             To   => "_");
   Syntax_Error                : exception;

   procedure Check_DAK (Network_Device_Name : String) is

      Is_Match : Boolean;

   begin

      Is_Match := Commands.Check_DAK (Network_Device_Name => Network_Device_Name,
                                      Pass_Phrase         => Ada.Command_Line.Argument (Number => 3));

      if Is_Match then
         Ada.Text_IO.Put_Line (Item => "Device Access Key matches");
      else
         Ada.Text_IO.Put_Line (Item => "Device Access Key does not match");
      end if;

   end Check_DAK;

   procedure Check_NMK (Network_Device_Name : String) is

      Is_Match : Boolean;

   begin

      Is_Match := Commands.Check_NMK (Network_Device_Name => Network_Device_Name,
                                      Pass_Phrase         => Ada.Command_Line.Argument (Number => 3));
      if Is_Match then
         Ada.Text_IO.Put_Line (Item => "Network Membership Key matches");
      else
         Ada.Text_IO.Put_Line (Item => "Network Membership Key does not match");
      end if;

   end Check_NMK;

   procedure Discover (Network_Device_Name : String) is

      Adapters : Power_Line_Adapter_Sets.Set (Capacity => Power_Line_Adapter.Max_Adapters);

   begin

      Adapters := Commands.Discover (Network_Device_Name => Network_Device_Name);

      for Adapter of Adapters loop
         Ada.Text_IO.Put_Line (Item => Adapter.Image);
      end loop;

   end Discover;

   procedure Get_Capabilities (Network_Device_Name : String) is

      Column_2     : constant                                      := 25;
      Capabilities : constant Power_Line_Adapter.Capabilities_Type := Commands.Get_Capabilities (Network_Device_Name => Network_Device_Name);

   begin

      Ada.Text_IO.Put (Item => "AV Version:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Image (HPAV_Version => Capabilities.AV_Version));
      Ada.Text_IO.Put (Item => "MAC Address:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Capabilities.MAC_Address.Image);
      Ada.Text_IO.Put (Item => "OUI:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put (Item => -(OUI_Format (Format => +"%06x", Var => Capabilities.OUI)));
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

   procedure Get_Discover_List (Network_Device_Name : String) is

      Column_2        : constant                       := 24;
      PLA_MAC_Address : MAC_Addresses.MAC_Address_Type := MAC_Addresses.Null_MAC_Address;

   begin

      if Ada.Command_Line.Argument_Count = 3 then
         PLA_MAC_Address := Get_PLA_MAC_Address (Image => Ada.Command_Line.Argument (Number => 3));
      end if;

      declare

         Discover_List : constant Power_Line_Adapter.Discover_List_Type := Commands.Get_Discover_List (Network_Device_Name => Network_Device_Name,
                                                                                                       PLA_MAC_Address     => PLA_MAC_Address);

      begin

         Ada.Text_IO.Put_Line (Item => "Number of stations:" & Discover_List.Number_Of_Stations'Image);

         for I in 1 .. Discover_List.Stations'Length loop
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

      end;

   end Get_Discover_List;

   procedure Get_HFID (Network_Device_Name : String) is

      HFID_Level : constant Commands.HFID_Level_Type := Get_HFID_Level;
      HFID       : HFID_String.Bounded_String;

   begin

      HFID := Commands.Get_HFID (Network_Device_Name => Network_Device_Name,
                                 HFID_Level          => HFID_Level);

      Ada.Text_IO.Put_Line (Item => HFID_String.To_String (Source => HFID));

   end Get_HFID;

   function Get_HFID_Level return Commands.HFID_Level_Type is

      HFID_Level_Arg : constant String := Ada.Command_Line.Argument (Number => 3);

   begin

      return Commands.HFID_Level_Type'Value (HFID_Level_Arg);

   exception

      when Constraint_Error =>
         raise Syntax_Error with "Invalid HFID level """ & HFID_Level_Arg & '"';

   end Get_HFID_Level;

   procedure Get_Id_Info (Network_Device_Name : String) is

      Column_2 : constant                                 := 22;
      Id_Info  : constant Power_Line_Adapter.Id_Info_Type := Commands.Get_Id_Info (Network_Device_Name => Network_Device_Name);

   begin

      Ada.Text_IO.Put (Item => "HomePlug AV Version:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Image (HPAV_Version => Id_Info.Homeplug_AV_Version));
      Ada.Text_IO.Put (Item => "MCS:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Id_Info.MCS'Image);

   end Get_Id_Info;

   procedure Get_Network_Info (Network_Device_Name : String) is

      Column_2          : constant                                           := 36;
      Network_Scope     : constant Commands.Network_Scope_Type               := Get_Network_Scope;
      Network_Info      : Power_Line_Adapter.Network_Info_Type;
      Network_Info_List : constant Power_Line_Adapter.Network_Info_List_Type := Commands.Get_Network_Info (Network_Device_Name => Network_Device_Name,
                                                                                                           Network_Scope       => Network_Scope);

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
         Ada.Text_IO.Put_Line (Item => Power_Line_Adapter.Station_Role_Type'Image (Network_Info_List (I).Station_Role));
         Ada.Text_IO.Put (Item => "  Network Kind:");
         Ada.Text_IO.Set_Col (To => Column_2);
         Ada.Text_IO.Put_Line (Item => Power_Line_Adapter.Network_Kind_Type'Image (Network_Info_List (I).Network_Kind));
         Ada.Text_IO.Put (Item => "  Status:");
         Ada.Text_IO.Set_Col (To => Column_2);
         Ada.Text_IO.Put_Line (Item => Power_Line_Adapter.Status_Type'Image (Network_Info_List (I).Status));
         Ada.Text_IO.New_Line (Spacing => 1);
      end loop;

   end Get_Network_Info;

   function Get_Network_Scope return Commands.Network_Scope_Type is

      Network_Scope_Arg : constant String := Ada.Command_Line.Argument (Number => 3);

   begin

      return Commands.Network_Scope_Type'Value (Network_Scope_Arg);

   exception

      when Constraint_Error =>
         raise Syntax_Error with "Invalid network scope """ & Network_Scope_Arg & '"';

   end Get_Network_Scope;

   procedure Get_Network_Stats (Network_Device_Name : String) is

      Column_2        : constant                       := 30;
      PLA_MAC_Address : MAC_Addresses.MAC_Address_Type := MAC_Addresses.Null_MAC_Address;

   begin

      if Ada.Command_Line.Argument_Count = 3 then
         PLA_MAC_Address := Get_PLA_MAC_Address (Image => Ada.Command_Line.Argument (Number => 3));
      end if;

      declare

         Network_Stats_List : constant Power_Line_Adapter.Network_Stats_List_Type := Commands.Get_Network_Stats (Network_Device_Name => Network_Device_Name,
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

      end;

   end Get_Network_Stats;

   function Get_PLA_MAC_Address (Image : String) return MAC_Addresses.MAC_Address_Type is
   begin

      return MAC_Addresses.Value (Image => Image);

   exception

      when Constraint_Error =>

         raise MAC_Addresses.MAC_Address_Error with "Invalid MAC address format " & Image;

   end Get_PLA_MAC_Address;

   function Image (HPAV_Version : Power_Line_Adapter.HPAV_Version_Type) return String is
   begin

      case HPAV_Version is
         when Power_Line_Adapter.HPAV_1_1 => return "1.1";
         when Power_Line_Adapter.HPAV_2_0 => return "2.0";
         when Power_Line_Adapter.Not_HPAV => return "Not a HomePlug AV device";
      end case;

   end Image;

   function Image (NID : Power_Line_Adapter.NID_Type) return String is
   begin

      return -(NID_Format (Format => +"%014x", Var => NID)) & " (" & Security_Level (NID => NID) & ')';

   end Image;

   function Image (Signal_Level : Power_Line_Adapter.Signal_Level_Type) return String is
   begin
      case Signal_Level is
         when 0 =>      return "not available";
         when 1 =>      return Integer'((Integer (Signal_Level) + 1) * (-5))'Image & " dB < SL <= 0 dB";
         when 15 =>     return "SL <= -75 dB";
         when others => return Integer'((Integer (Signal_Level) + 1) * (-5))'Image & " dB < SL <= " & Integer'(Integer (Signal_Level) * (-5))'Image & " dB";
      end case;

   end Image;

   procedure Process_Command_Line is

   begin

      case Ada.Command_Line.Argument_Count is
         when 0 =>
            Show_Version;
            Show_Help;
            return;
         when 1 =>
            raise Syntax_Error with Message_Too_Few_Arguments;
         when others =>
            null;
      end case;

      declare

         Command             : Commands.Command_Type;
         Command_Id          : constant String := Ada.Command_Line.Argument (Number => 2);
         Network_Device_Name : constant String := Ada.Command_Line.Argument (Number => 1);

      begin

         Command := To_Command (Source => Command_Id);

         case Command is
            when Commands.Check_DAK =>

               if Ada.Command_Line.Argument_Count < 3 then
                  raise Syntax_Error with Message_Too_Few_Arguments;
               end if;

               Check_DAK (Network_Device_Name => Network_Device_Name);

            when Commands.Check_NMK =>

               if Ada.Command_Line.Argument_Count < 3 then
                  raise Syntax_Error with Message_Too_Few_Arguments;
               end if;

               Check_NMK (Network_Device_Name => Network_Device_Name);

            when Commands.Discover =>

               Discover (Network_Device_Name => Network_Device_Name);

            when Commands.Get_Capabilities =>

               Get_Capabilities (Network_Device_Name => Network_Device_Name);

            when Commands.Get_Discover_List =>

               Get_Discover_List (Network_Device_Name => Network_Device_Name);

            when Commands.Get_HFID =>

               if Ada.Command_Line.Argument_Count < 3 then
                  raise Syntax_Error with Message_Too_Few_Arguments;
               end if;

               Get_HFID (Network_Device_Name => Network_Device_Name);

            when Commands.Get_Id_Info =>

               Get_Id_Info (Network_Device_Name => Network_Device_Name);

            when Commands.Get_Network_Info =>

               if Ada.Command_Line.Argument_Count < 3 then
                  raise Syntax_Error with Message_Too_Few_Arguments;
               end if;

               Get_Network_Info (Network_Device_Name => Network_Device_Name);

            when Commands.Get_Network_Stats =>

               Get_Network_Stats (Network_Device_Name => Network_Device_Name);

            when Commands.Reset =>

               if Ada.Command_Line.Argument_Count < 3 then
                  raise Syntax_Error with Message_Too_Few_Arguments;
               end if;

               Reset (Network_Device_Name => Network_Device_Name);

            when Commands.Restart =>

               if Ada.Command_Line.Argument_Count < 3 then
                  raise Syntax_Error with Message_Too_Few_Arguments;
               end if;

               Restart (Network_Device_Name => Network_Device_Name);

            when Commands.Set_HFID =>

               if Ada.Command_Line.Argument_Count < 3 then
                  raise Syntax_Error with Message_Too_Few_Arguments;
               end if;

               Set_HFID (Network_Device_Name => Network_Device_Name);

            when Commands.Set_NMK =>

               if Ada.Command_Line.Argument_Count < 3 then
                  raise Syntax_Error with Message_Too_Few_Arguments;
               end if;

               Set_NMK (Network_Device_Name => Network_Device_Name);
         end case;

      end;

   exception

      when Error : Syntax_Error =>

         Ada.Text_IO.Put_Line (Item => "Error: " & Ada.Exceptions.Exception_Message (X => Error));
         Ada.Text_IO.New_Line (Spacing => 1);
         Show_Help;

      when Error : Commands.Command_Error | Power_Line_Adapter.Adapter_Error | MAC_Addresses.MAC_Address_Error =>

         Ada.Text_IO.Put_Line (Item => "Error: " & Ada.Exceptions.Exception_Message (X => Error));

   end Process_Command_Line;

   procedure Reset (Network_Device_Name : String) is

      PLA_MAC_Address : constant MAC_Addresses.MAC_Address_Type := Get_PLA_MAC_Address (Image => Ada.Command_Line.Argument (Number => 3));

   begin

      Commands.Reset (Network_Device_Name => Network_Device_Name,
                      PLA_MAC_Address     => PLA_MAC_Address);

      Ada.Text_IO.Put_Line (Item => "Device was reset to factory defaults");

   end Reset;

   procedure Restart (Network_Device_Name : String) is

      PLA_MAC_Address : constant MAC_Addresses.MAC_Address_Type := Get_PLA_MAC_Address (Image => Ada.Command_Line.Argument (Number => 3));

   begin

      Commands.Restart (Network_Device_Name => Network_Device_Name,
                        PLA_MAC_Address     => PLA_MAC_Address);

      Ada.Text_IO.Put_Line (Item => "Device was restarted");

   end Restart;

   function Security_Level (NID : Power_Line_Adapter.NID_Type) return String is
   begin

      return (if (NID and 16#10_0000_0000_0000#) = 0 then "SL-SC" else "SL-HS");

   end Security_Level;

   procedure Set_HFID (Network_Device_Name : String) is

      HFID : constant HFID_String.Bounded_String := HFID_String.To_Bounded_String (Source => Ada.Command_Line.Argument (Number => 3),
                                                                                   Drop   => Ada.Strings.Error);

   begin

      Commands.Set_HFID (Network_Device_Name => Network_Device_Name,
                         HFID                => HFID);

      Ada.Text_IO.Put_Line (Item => "User HFID set");

   end Set_HFID;

   procedure Set_NMK (Network_Device_Name : String) is

   begin

      Commands.Set_NMK (Network_Device_Name => Network_Device_Name,
                        Pass_Phrase         => Ada.Command_Line.Argument (Number => 3));

      Ada.Text_IO.Put_Line (Item => "Network Membership Key set");

   end Set_NMK;

   procedure Show_Help is
   begin
      Ada.Text_IO.Put_Line (Item => "Try one of the following commands:");
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> discover");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> reset <pla-mac-address>");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> restart <pla-mac-address>");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> get-capabilities");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> get-discover-list [pla-mac-address]");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> get-hfid manufacturer");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> get-hfid user");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> get-id-info");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> get-network-info member");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> get-network-info any");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> get-network-stats [pla-mac-address]");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> set-hfid <id>");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> set-nmk <pass-phrase>");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> check-dak <plc-pass-phrase>");
      Ada.Text_IO.Put_Line (Item => "pla-util <NIC> check-nmk <pass-phrase>");
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put_Line (Item => "where <NIC> is the name of an ethernet network device");
   end Show_Help;

   procedure Show_Version is
   begin
      Ada.Text_IO.Put_Line (Item => "pla-util version 1.2.0");
      Ada.Text_IO.New_Line (Spacing => 1);
   end Show_Version;

   function To_Command (Source : String) return Commands.Command_Type is

      Command      : Commands.Command_Type;
      Command_Name : constant String := Ada.Strings.Fixed.Translate (Source  => Source,
                                                                     Mapping => Separator_Character_Mapping);

   begin

      Command := Commands.Command_Type'Value (Command_Name);

      return Command;

   exception

      when Constraint_Error =>
         raise Syntax_Error with "Invalid command """ & Source & '"';

   end To_Command;

end Console;
