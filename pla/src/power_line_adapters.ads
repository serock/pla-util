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
with Firmware_Boot_Strings;
with HFID_Strings;
with MAC_Addresses;
with Messages;
private with Ada.Streams;
private with Octets;
private with Packets;

package Power_Line_Adapters is

   type Adapter_Type                is tagged private;
   type Capable_Type                is (Not_Capable, Capable);
   type Chip_Id_Type                is mod 16#1_0000_0000#;
   type Chip_Version_Type           is (Unknown, BCM60500_A0, BCM60500_A1, BCM60500_B0, BCM60333_A1, BCM60333_B0, BCM60335_A0);
   type Coordinating_Status_Type    is (Unknown, Non_Coordinating_Network, Coordinating_Network, Coordinating_Network_In_Same_Group, Coordinating_Network_In_Different_Group);
   type Data_Rate_Type              is mod 2048;
   type Firmware_Name_Type          is (INVALID, APOLLO, CONCORDE, GEMINI, HYDRA);
   type Firmware_Revision_Type      is mod 16#1_0000_0000#;
   type Flash_Model_Type            is (UNKNOWN, DEFAULT, S25FL216K, EN25F80, SST25VF016B, SST25VF032B, SST25VF080B, MX25L8006E, MX25L1606E, MX25L3206E,
                                        GD25Q80B, GD25Q16B, GD25Q32B, W25Q80BV, W25Q16BV, W25Q32BV, FM25S16);
   type Hardware_Version_Type       is mod 16#1_0000_0000#;
   type Homeplug_Version_Type       is (Unknown, V_1_1, V_2_0);
   type HPAV_Version_Type           is (HPAV_1_1, HPAV_2_0, Not_HPAV);
   type Implementation_Version_Type is mod 65536;
   type Max_Bit_Rate_Type           is (Unknown, MBR_200, MBR_1000, MBR_1800);
   type MCS_Type                    is (MIMO_Not_Supported, Selection_Diversity, MIMO_With_Beam_Forming);
   type Network_Kind_Type           is (In_Home_Network, Access_Network);
   type Network_Interface_Type      is (MII0, MII1, PLC, SDR);
   type NID_Type                    is mod 16#40_0000_0000_0000#;
   type No_Yes_Type                 is (No, Yes);
   type OUI_Type                    is mod 16#100_0000#;
   type Param_Config_Version_Type   is mod 16#1_0000_0000#;
   type Partial_Version_Type        is mod 64;
   type Signal_Level_Type           is mod 16;
   type SNID_Type                   is mod 16;
   type Station_Role_Type           is (Unassoc_STA, Unassoc_CCo, STA, CCo, Backup_CCo);
   type Status_Type                 is (Joined, Not_Joined_Have_NMK, Not_Joined_No_NMK);
   type TEI_Type                    is mod 256;
   type Uptime_Type                 is mod 16#1_0000_0000#;

   subtype AV_Version_Type    is HPAV_Version_Type range HPAV_1_1 .. HPAV_2_0;
   subtype Network_Count_Type is Natural range 0 .. 6;
   subtype Station_Count_Type is Natural range 0 .. 16;

   type Capabilities_Type is
      record
         AV_Version             : AV_Version_Type;
         MAC_Address            : MAC_Addresses.MAC_Address_Type;
         OUI                    : OUI_Type;
         Backup_CCo             : Capable_Type;
         Proxy                  : Capable_Type;
         Implementation_Version : Implementation_Version_Type;
      end record;

   type Firmware_Version_Type is
      record
         Name  : Firmware_Name_Type;
         Major : Partial_Version_Type;
         Minor : Partial_Version_Type;
         Build : Partial_Version_Type;
      end record;

   type Id_Info_Type is
      record
         Homeplug_AV_Version : HPAV_Version_Type;
         MCS                 : MCS_Type;
      end record;

   type Network_Info_Type is
      record
         NID                : NID_Type;
         SNID               : SNID_Type;
         TEI                : TEI_Type;
         CCo_MAC_Address    : MAC_Addresses.MAC_Address_Type;
         BCCo_MAC_Address   : MAC_Addresses.MAC_Address_Type;
         Num_Coord_Networks : Network_Count_Type;
         Station_Role       : Station_Role_Type;
         Network_Kind       : Network_Kind_Type;
         Status             : Status_Type;
      end record;

   type Network_Info_List_Type is array (Positive range <>) of Network_Info_Type;

   type Network_Type is
      record
         NID                 : NID_Type;
         SNID                : SNID_Type;
         Coordinating_Status : Coordinating_Status_Type;
      end record;

   type Network_List_Type is array (Positive range <>) of Network_Type;

   type Network_Stats_Type is
      record
         Destination_Address    : MAC_Addresses.MAC_Address_Type;
         Average_Rate_To_Dest   : Data_Rate_Type;
         Average_Rate_From_Dest : Data_Rate_Type;
      end record;

   type Network_Stats_List_Type is array (Positive range <>) of Network_Stats_Type;

   type ROM_Version_Type is
      record
         Major : Partial_Version_Type;
         Minor : Partial_Version_Type;
         Build : Partial_Version_Type;
      end record;

   type Station_Type is
      record
         MAC_Address  : MAC_Addresses.MAC_Address_Type;
         TEI          : TEI_Type;
         Same_Network : No_Yes_Type;
         SNID         : SNID_Type;
         CCo          : No_Yes_Type;
         PCo          : No_Yes_Type;
         Backup_CCo   : No_Yes_Type;
         Signal_Level : Signal_Level_Type;
      end record;

   type Station_Info_Type is
      record
         Chip_Version                  : Chip_Version_Type;
         Hardware_Version              : Hardware_Version_Type;
         Firmware_Revision             : Firmware_Revision_Type;
         Chip_Id                       : Chip_Id_Type;
         ROM_Version                   : ROM_Version_Type;
         Param_Config_Built_In_Version : Param_Config_Version_Type;
         Param_Config_NVM_Version      : Param_Config_Version_Type;
         Uptime                        : Uptime_Type;
         Firmware_Boot_Message         : Firmware_Boot_Strings.Bounded_String;
         Firmware_Version              : Firmware_Version_Type;
         Flash_Model                   : Flash_Model_Type;
         Homeplug_Version              : Homeplug_Version_Type;
         Max_Bit_Rate                  : Max_Bit_Rate_Type;
      end record;

   type Station_List_Type is array (Positive range <>) of Station_Type;

   type Discover_List_Type (Number_Of_Stations : Station_Count_Type; Number_Of_Networks : Network_Count_Type) is record
      Stations : Station_List_Type (1 .. Number_Of_Stations);
      Networks : Network_List_Type (1 .. Number_Of_Networks);
   end record;

   Max_Adapters : constant := 16;

   Adapter_Error : exception;

   function "<" (Left  : Adapter_Type;
                 Right : Adapter_Type) return Boolean;

   overriding function "=" (Left  : Adapter_Type;
                            Right : Adapter_Type) return Boolean;

   function Check_DAK (Self       : Adapter_Type;
                       Passphrase : String) return Boolean;

   function Check_NMK (Self       : Adapter_Type;
                       Passphrase : String) return Boolean;

   function Get_Any_Network_Info (Self : Adapter_Type) return Network_Info_List_Type;

   function Get_Capabilities (Self : Adapter_Type) return Capabilities_Type;

   function Get_Discover_List (Self : Adapter_Type) return Discover_List_Type;

   function Get_Id_Info (Self : Adapter_Type) return Id_Info_Type;

   function Get_Manufacturer_HFID (Self : Adapter_Type) return HFID_Strings.Bounded_String;

   function Get_Member_Network_Info (Self : Adapter_Type) return Network_Info_List_Type;

   function Get_Network_Stats (Self : Adapter_Type) return Network_Stats_List_Type;

   function Get_Station_Info (Self : Adapter_Type) return Station_Info_Type;

   function Get_User_HFID (Self : Adapter_Type) return HFID_Strings.Bounded_String;

   function Has_MAC_Address (Self        : Adapter_Type;
                             MAC_Address : MAC_Addresses.MAC_Address_Type) return Boolean;

   function Image (Self : Adapter_Type) return String;

   procedure Reset (Self : Adapter_Type);

   procedure Restart (Self : Adapter_Type);

   procedure Set_HFID (Self : Adapter_Type;
                       HFID : HFID_Strings.Bounded_String);

   procedure Set_NMK (Self       : Adapter_Type;
                      Passphrase : String);

private

   Message_No_Confirmation         : constant String := "No confirmation received from adapter";
   Message_Unexpected_Confirmation : constant String := "Unexpected confirmation received from adapter";

   type HFID_Kind_Type     is (MANUFACTURER, USER);
   type Network_Scope_Type is (MEMBER, ANY);

   for Partial_Version_Type'Size use 8;

   function Generate_DAK (Passphrase : String) return Octets.Key_Type;

   function Generate_Key (Passphrase : String;
                          Salt       : Ada.Streams.Stream_Element_Array) return Octets.Key_Type;

   function Generate_NMK (Passphrase : String) return Octets.Key_Type;

   function To_HFID_String (HFID_Octets : Octets.Octets_Type) return HFID_Strings.Bounded_String;

   procedure Validate_HFID (HFID            : HFID_Strings.Bounded_String;
                            Min_HFID_Length : Positive := 1);

   type Adapter_Type is tagged
      record
         Network_Interface : Network_Interface_Type;
         MAC_Address       : MAC_Addresses.MAC_Address_Type;
         HFID              : HFID_Strings.Bounded_String;
      end record;

   procedure Initialize (Self              : out Adapter_Type;
                         Network_Interface :     Network_Interface_Type;
                         MAC_Address       :     MAC_Addresses.MAC_Address_Type;
                         HFID              :     HFID_Strings.Bounded_String);

   function Get_HFID (Self : Adapter_Type;
                      Kind : HFID_Kind_Type) return HFID_Strings.Bounded_String;

   function Get_Network_Info (Self  : Adapter_Type;
                              Scope : Network_Scope_Type) return Network_Info_List_Type;

   procedure Process (Self                :     Adapter_Type;
                      Request             :     Messages.Message_Type;
                      Confirmation        : out Packets.Payload_Type;
                      Confirmation_Length : out Natural;
                      From_MAC_Address    : out MAC_Addresses.MAC_Address_Type);

   procedure Validate_DAK_Passphrase (Passphrase       : String;
                                      Check_Min_Length : Boolean := True);

   procedure Validate_NMK_Passphrase (Passphrase       : String;
                                      Check_Min_Length : Boolean := True);

   procedure Validate_Passphrase (Passphrase            : String;
                                  Min_Passphrase_Length : Positive;
                                  Max_Passphrase_Length : Positive;
                                  Check_Min_Length      : Boolean := True);

end Power_Line_Adapters;
