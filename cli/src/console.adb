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
with Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Config;
with GNAT.Directory_Operations;
with GNAT.Formatted_String;
with HFID_Strings;
with Power_Line_Adapter_Sets;
with Power_Line_Adapters;

use GNAT.Formatted_String;
use type Ada.Text_IO.Count;
use type Power_Line_Adapters.NID_Type;

package body Console is

   package Data_Rate_Text_IO     is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.Data_Rate_Type);
   package Network_Count_Text_IO is new Ada.Text_IO.Integer_IO (Num => Power_Line_Adapters.Network_Count_Type);
   package NID_Text_IO           is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.NID_Type);
   package OUI_Text_IO           is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.OUI_Type);
   package SNID_Text_IO          is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.SNID_Type);
   package TEI_Text_IO           is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.TEI_Type);

   function NID_Format is new GNAT.Formatted_String.Mod_Format (Int => Power_Line_Adapters.NID_Type,
                                                                Put => NID_Text_IO.Put);

   function OUI_Format is new GNAT.Formatted_String.Mod_Format (Int => Power_Line_Adapters.OUI_Type,
                                                                Put => OUI_Text_IO.Put);

   App_Name                    : constant String                             := GNAT.Directory_Operations.Base_Name (Ada.Command_Line.Command_Name);
   Separator_Character_Mapping : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping (From => "-",
                                                                                                             To   => "_");
   Syntax_Error                : exception;
   Version                     : constant String                             := "2.0.0";

   procedure Check_DAK (Network_Device_Name : String;
                        Passphrase          : String;
                        PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is

      Is_Match : constant Boolean := Commands.Check_DAK (Network_Device_Name => Network_Device_Name,
                                                         Passphrase          => Passphrase,
                                                         PLA_MAC_Address     => PLA_MAC_Address);

   begin

      if Is_Match then
         Ada.Text_IO.Put_Line (Item => "Device Access Key matches");
      else
         Ada.Text_IO.Put_Line (Item => "Device Access Key does not match");
      end if;

   end Check_DAK;

   procedure Check_NMK (Network_Device_Name : String;
                        Passphrase          : String;
                        PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is

      Is_Match : constant Boolean := Commands.Check_NMK (Network_Device_Name => Network_Device_Name,
                                                         Passphrase          => Passphrase,
                                                         PLA_MAC_Address     => PLA_MAC_Address);

   begin

      if Is_Match then
         Ada.Text_IO.Put_Line (Item => "Network Membership Key matches");
      else
         Ada.Text_IO.Put_Line (Item => "Network Membership Key does not match");
      end if;

   end Check_NMK;

   procedure Discover (Network_Device_Name : String;
                       PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is

      Adapters : constant Power_Line_Adapter_Sets.Set := Commands.Discover (Network_Device_Name => Network_Device_Name,
                                                                            PLA_MAC_Address     => PLA_MAC_Address);

   begin

      for Adapter of Adapters loop
         Ada.Text_IO.Put_Line (Item => Adapter.Image);
      end loop;

   end Discover;

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

   procedure Get_HFID (Network_Device_Name : String;
                       HFID_Level          : Commands.HFID_Level_Type;
                       PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is

      HFID : HFID_Strings.Bounded_String;

   begin

      HFID := Commands.Get_HFID (Network_Device_Name => Network_Device_Name,
                                 HFID_Level          => HFID_Level,
                                 PLA_MAC_Address     => PLA_MAC_Address);

      Ada.Text_IO.Put_Line (Item => HFID_Strings.To_String (Source => HFID));

   end Get_HFID;

   function Get_HFID_Level (HFID_Level_Text : String) return Commands.HFID_Level_Type is
   begin

      return Commands.HFID_Level_Type'Value (HFID_Level_Text);

   exception

      when Constraint_Error =>
         raise Syntax_Error with "Invalid HFID level """ & HFID_Level_Text & '"';

   end Get_HFID_Level;

   procedure Get_Id_Info (Network_Device_Name : String;
                          PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is

      Column_2 : constant := 22;
      Id_Info  : constant Power_Line_Adapters.Id_Info_Type := Commands.Get_Id_Info (Network_Device_Name => Network_Device_Name,
                                                                                    PLA_MAC_Address     => PLA_MAC_Address);

   begin

      Ada.Text_IO.Put (Item => "HomePlug AV Version:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Image (HPAV_Version => Id_Info.Homeplug_AV_Version));
      Ada.Text_IO.Put (Item => "MCS:");
      Ada.Text_IO.Set_Col (To => Column_2);
      Ada.Text_IO.Put_Line (Item => Id_Info.MCS'Image);

   end Get_Id_Info;

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

   function Get_Network_Scope (Network_Scope_Text : String) return Commands.Network_Scope_Type is
   begin

      return Commands.Network_Scope_Type'Value (Network_Scope_Text);

   exception

      when Constraint_Error =>
         raise Syntax_Error with "Invalid network scope """ & Network_Scope_Text & '"';

   end Get_Network_Scope;

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

   function Get_PLA_MAC_Address (MAC_Address_Image : String) return MAC_Addresses.MAC_Address_Type is
   begin

      return MAC_Addresses.Value (MAC_Address_Image => MAC_Address_Image);

   exception

      when Constraint_Error =>

         raise MAC_Addresses.MAC_Address_Error with "Invalid MAC address '" & MAC_Address_Image & "'";

   end Get_PLA_MAC_Address;

   procedure Get_Station_Info (Network_Device_Name : String;
                               PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is
   begin

      Commands.Get_Station_Info (Network_Device_Name => Network_Device_Name,
                                 PLA_MAC_Address     => PLA_MAC_Address);

      Ada.Text_IO.Put_Line (Item => "Get_Station_Info command was sent to device");

   end Get_Station_Info;

   function Image (HPAV_Version : Power_Line_Adapters.HPAV_Version_Type) return String is
   begin

      case HPAV_Version is
         when Power_Line_Adapters.HPAV_1_1 => return "1.1";
         when Power_Line_Adapters.HPAV_2_0 => return "2.0";
         when Power_Line_Adapters.Not_HPAV => return "Not a HomePlug AV device";
      end case;

   end Image;

   function Image (NID : Power_Line_Adapters.NID_Type) return String is
   begin

      return -(NID_Format (Format => +"%014x", Var => NID)) & " (" & Security_Level (NID => NID) & ')';

   end Image;

   function Image (Signal_Level : Power_Line_Adapters.Signal_Level_Type) return String is
   begin
      case Signal_Level is
         when 0 =>      return "not available";
         when 1 =>      return Integer'Image ((Integer (Signal_Level) + 1) * (-5)) & " dB < SL <= 0 dB";
         when 15 =>     return "SL <= -75 dB";
         when others => return Integer'Image ((Integer (Signal_Level) + 1) * (-5)) & " dB < SL <= " & Integer'Image (Integer (Signal_Level) * (-5)) & " dB";
      end case;

   end Image;

   procedure Process_Command_Line is separate;

   procedure Reset (Network_Device_Name : String;
                    PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is
   begin

      Commands.Reset (Network_Device_Name => Network_Device_Name,
                      PLA_MAC_Address     => PLA_MAC_Address);

      Ada.Text_IO.Put_Line (Item => "Device was reset to factory defaults");

   end Reset;

   procedure Restart (Network_Device_Name : String;
                      PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is
   begin

      Commands.Restart (Network_Device_Name => Network_Device_Name,
                        PLA_MAC_Address     => PLA_MAC_Address);

      Ada.Text_IO.Put_Line (Item => "Device was restarted");

   end Restart;

   function Security_Level (NID : Power_Line_Adapters.NID_Type) return String is
   begin

      return (if (NID and 16#10_0000_0000_0000#) = 0 then "SL-SC" else "SL-HS");

   end Security_Level;

   procedure Set_HFID (Network_Device_Name : String;
                       HFID                : String;
                       PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is
   begin

      Commands.Set_HFID (Network_Device_Name => Network_Device_Name,
                         HFID                => HFID_Strings.To_Bounded_String (Source => HFID,
                                                                                Drop => Ada.Strings.Error),
                         PLA_MAC_Address     => PLA_MAC_Address);

      Ada.Text_IO.Put_Line (Item => "User HFID set");

   end Set_HFID;

   procedure Set_NMK (Network_Device_Name : String;
                      Passphrase          : String;
                      PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is
   begin

      Commands.Set_NMK (Network_Device_Name => Network_Device_Name,
                        Passphrase          => Passphrase,
                        PLA_MAC_Address     => PLA_MAC_Address);

      Ada.Text_IO.Put_Line (Item => "Network Membership Key set");

   end Set_NMK;

   procedure Show_Help is
   begin
      Show_Version;
      Ada.Text_IO.Put_Line (Item => "A utility for power line adapters with Broadcom chipsets");
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put_Line (Item => "Usage:");
      Ada.Text_IO.Put_Line (Item => "  " & App_Name & " [options] <command> [arguments]");
      Ada.Text_IO.Put_Line (Item => "  " & App_Name & " -h | --help                      Display help and exit");
      Ada.Text_IO.Put_Line (Item => "  " & App_Name & " -V | --version                   Display version and exit");
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put_Line (Item => "Options:");
      Ada.Text_IO.Put_Line (Item => "  -i, --interface=<name>   Network interface to use (e.g., eth0)");
      Ada.Text_IO.Put_Line (Item => "  -p, --pla=<mac-address>  Power line adapter at unicast MAC address");
      Ada.Text_IO.Put_Line (Item => "  -t, --timeout=<ms>       Network timeout in milliseconds [default:" & Positive'Image (Config.Network_Receive_Timeout) & "]");
      Ada.Text_IO.New_Line (Spacing => 1);
      Ada.Text_IO.Put_Line (Item => "Commands:");
      Ada.Text_IO.Put_Line (Item => "  check-dak <pla-passphrase>         Check device access key");
      Ada.Text_IO.Put_Line (Item => "  check-nmk <passphrase>             Check network membership key");
      Ada.Text_IO.Put_Line (Item => "  discover                           Discover power line adapters on subnet");
      Ada.Text_IO.Put_Line (Item => "  get-capabilities                   Get capabilities");
      Ada.Text_IO.Put_Line (Item => "  get-discover-list                  Get discovered PLAs and networks");
      Ada.Text_IO.Put_Line (Item => "  get-hfid ( manufacturer | user )   Get human-friendly id [default: user]");
      Ada.Text_IO.Put_Line (Item => "  get-id-info                        Get identification info");
      Ada.Text_IO.Put_Line (Item => "  get-network-info ( any | member )  Get network information [default: member]");
      Ada.Text_IO.Put_Line (Item => "  get-network-stats                  Get average PHY data rates");
      Ada.Text_IO.Put_Line (Item => "  get-station_info                   Get power line adapter information");
      Ada.Text_IO.Put_Line (Item => "  reset                              Factory reset power line adapter");
      Ada.Text_IO.Put_Line (Item => "  restart                            Restart / reboot power line adapter");
      Ada.Text_IO.Put_Line (Item => "  set-hfid <id>                      Set user human-friendly id");
      Ada.Text_IO.Put_Line (Item => "  set-nmk <passphrase>               Set network membership key");
   end Show_Help;

   procedure Show_Version is
   begin
      Ada.Text_IO.Put_Line (Item => App_Name & " " & Version);
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
         raise Syntax_Error with "Invalid command '" & Source & "'";

   end To_Command;

end Console;
