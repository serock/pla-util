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

   package Chip_Id_Text_IO              is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.Chip_Id_Type);
   package Data_Rate_Text_IO            is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.Data_Rate_Type);
   package Hardware_Version_Text_IO     is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.Hardware_Version_Type);
   package Network_Count_Text_IO        is new Ada.Text_IO.Integer_IO (Num => Power_Line_Adapters.Network_Count_Type);
   package NID_Text_IO                  is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.NID_Type);
   package Param_Config_Version_Text_IO is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.Param_Config_Version_Type);
   package Partial_Version_Text_IO      is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.Partial_Version_Type);
   package SNID_Text_IO                 is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.SNID_Type);
   package TEI_Text_IO                  is new Ada.Text_IO.Modular_IO (Num => Power_Line_Adapters.TEI_Type);

   function Chip_Id_Format is new GNAT.Formatted_String.Mod_Format (Int => Power_Line_Adapters.Chip_Id_Type,
                                                                    Put => Chip_Id_Text_IO.Put);

   function Hardware_Version_Format is new GNAT.Formatted_String.Mod_Format (Int => Power_Line_Adapters.Hardware_Version_Type,
                                                                             Put => Hardware_Version_Text_IO.Put);

   function NID_Format is new GNAT.Formatted_String.Mod_Format (Int => Power_Line_Adapters.NID_Type,
                                                                Put => NID_Text_IO.Put);

   App_Name                    : constant String                             := GNAT.Directory_Operations.Base_Name (Ada.Command_Line.Command_Name);
   Separator_Character_Mapping : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping (From => "-",
                                                                                                             To   => "_");
   Syntax_Error                : exception;
   Version                     : constant String                             := "2.1.0";

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
                               PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is separate;

   procedure Get_Discover_List (Network_Device_Name : String;
                                PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is separate;

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
                               PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is separate;

   function Get_Network_Scope (Network_Scope_Text : String) return Commands.Network_Scope_Type is
   begin

      return Commands.Network_Scope_Type'Value (Network_Scope_Text);

   exception

      when Constraint_Error =>
         raise Syntax_Error with "Invalid network scope """ & Network_Scope_Text & '"';

   end Get_Network_Scope;

   procedure Get_Network_Stats (Network_Device_Name : String;
                                PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is separate;

   function Get_PLA_MAC_Address (MAC_Address_Image : String) return MAC_Addresses.MAC_Address_Type is
   begin

      return MAC_Addresses.Value (MAC_Address_Image => MAC_Address_Image);

   exception

      when Constraint_Error =>

         raise MAC_Addresses.MAC_Address_Error with "Invalid MAC address '" & MAC_Address_Image & "'";

   end Get_PLA_MAC_Address;

   procedure Get_Station_Info (Network_Device_Name : String;
                               PLA_MAC_Address     : MAC_Addresses.MAC_Address_Type) is separate;

   function Image (Chip_Id : Power_Line_Adapters.Chip_Id_Type) return String is
   begin

      return "0x" & (-(Chip_Id_Format (Format => +"%08x", Var => Chip_Id)));

   end Image;

   function Image (Hardware_Version : Power_Line_Adapters.Hardware_Version_Type) return String is
   begin

      return "0x" & (-(Hardware_Version_Format (Format => +"%08x", Var => Hardware_Version)));

   end Image;

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

   function Image (Uptime : Power_Line_Adapters.Uptime_Type) return String is

      Days      : Natural;
      Hours     : Natural;
      Minutes   : Natural;
      Seconds   : Natural;
      Remainder : Natural;

      use type Power_Line_Adapters.Uptime_Type;

   begin

      Days      := Natural (Uptime / 86400);
      Remainder := Natural (Uptime rem 86400);
      Hours     := Remainder / 3600;
      Remainder := Remainder rem 3600;
      Minutes   := Remainder / 60;
      Seconds   := Remainder rem 60;

      return -(+"%u %02u:%02u:%02u" & Days & Hours & Minutes & Seconds);

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

   procedure Show_Help is separate;

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
