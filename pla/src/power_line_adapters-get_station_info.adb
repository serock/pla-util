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
with Ada.Exceptions;
with Messages.Constructors;

separate (Power_Line_Adapters)

function Get_Station_Info (Self : Adapter_Type) return Station_Info_Type is

   Confirmation          : Packets.Payload_Type (1 .. 358);
   Confirmation_Length   : Natural;
   Expected_Confirmation : constant Packets.Payload_Type := (16#02#, 16#4d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#);
   MAC_Address           : MAC_Addresses.MAC_Address_Type;

   function To_String is new To_Bounded_String (T      => Firmware_Boot_Strings.Bounded_String,
                                                Append => Firmware_Boot_Strings.Append);

begin

   declare
      use type Octets.Octets_Type;

      Request : constant Messages.Message_Type := Messages.Constructors.Create_Get_Station_Info_Request;
   begin

      Self.Process (Request             => Request,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

      if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

   end;

   declare
      use type Octets.Octet_Type;

      type Chip_Version_Id_Type is mod 16#1_0000_0000#;
      type Flash_Model_Id_Type  is mod 16#1_0000_0000#;

      Chip_Version_Id  : constant Chip_Version_Id_Type := Chip_Version_Id_Type (Confirmation (10)) + 256 * Chip_Version_Id_Type (Confirmation (11)) +
                                                          65536 * Chip_Version_Id_Type (Confirmation (12)) + 16777216 * Chip_Version_Id_Type (Confirmation (13));
      Number_Of_Ucodes : constant Natural := Natural (Confirmation (34));
      Station_Info     : Station_Info_Type;
      I                : Natural          := 35 + 5 * Number_Of_Ucodes;
   begin

      case Chip_Version_Id is
         when 16#017f0000# => Station_Info.Chip_Version := BCM60500_A0;
         when 16#017f024e# => Station_Info.Chip_Version := BCM60500_A1;
         when 16#117f024e# => Station_Info.Chip_Version := BCM60500_B0;
         when 16#017f024f# => Station_Info.Chip_Version := BCM60333_A1;
         when 16#117f024f# => Station_Info.Chip_Version := BCM60333_B0;
         when 16#017f025a# => Station_Info.Chip_Version := BCM60335_A0;
         when others       => Station_Info.Chip_Version := Unknown;
      end case;

      Station_Info.Hardware_Version := Hardware_Version_Type (Confirmation (14)) + 256 * Hardware_Version_Type (Confirmation (15)) +
                                       65536 * Hardware_Version_Type (Confirmation (16)) + 16777216 * Hardware_Version_Type (Confirmation (17));

      Station_Info.Firmware_Revision := Firmware_Revision_Type (Confirmation (18)) + 256 * Firmware_Revision_Type (Confirmation (19)) +
                                        65536 * Firmware_Revision_Type (Confirmation (20)) + 16777216 * Firmware_Revision_Type (Confirmation (21));

      Station_Info.Chip_Id := Chip_Id_Type (Confirmation (22)) + 256 * Chip_Id_Type (Confirmation (23)) +
                              65536 * Chip_Id_Type (Confirmation (24)) + 16777216 * Chip_Id_Type (Confirmation (25));

      Station_Info.ROM_Version.Build := Partial_Version_Type (Confirmation (22) and 16#3f#);
      Station_Info.ROM_Version.Minor := Partial_Version_Type ((Confirmation (22) and 16#c0#) / 64) + Partial_Version_Type ((Confirmation (23) and 16#0f#) * 4);
      Station_Info.ROM_Version.Major := Partial_Version_Type (Confirmation (23) / 16);

      Station_Info.Param_Config_Built_In_Version := Param_Config_Version_Type (Confirmation (26)) + 256 * Param_Config_Version_Type (Confirmation (27)) +
                                                    65536 * Param_Config_Version_Type (Confirmation (28)) + 16777216 * Param_Config_Version_Type (Confirmation (29));

      Station_Info.Param_Config_NVM_Version := Param_Config_Version_Type (Confirmation (30)) + 256 * Param_Config_Version_Type (Confirmation (31)) +
                                               65536 * Param_Config_Version_Type (Confirmation (32)) + 16777216 * Param_Config_Version_Type (Confirmation (33));

      Station_Info.Uptime := Uptime_Type (Confirmation (I)) + 256 * Uptime_Type (Confirmation (I + 1)) +
                             65536 * Uptime_Type (Confirmation (I + 2)) + 16777216 * Uptime_Type (Confirmation (I + 3));

      I := I + 4;

      declare
         Boot_Message_Length : constant Natural := Natural (Confirmation (I));
      begin
         I := I + 1;
         Station_Info.Firmware_Boot_Message := To_String (Data => Confirmation (I .. I + Boot_Message_Length - 1));
         I := I + Boot_Message_Length;
      end;

      case Confirmation (I + 3) is
         when 0      => Station_Info.Firmware_Version.Name := CONCORDE;
         when 5      => Station_Info.Firmware_Version.Name := GEMINI;
         when 6      => Station_Info.Firmware_Version.Name := APOLLO;
         when 7      => Station_Info.Firmware_Version.Name := HYDRA;
         when others => Station_Info.Firmware_Version.Name := INVALID;
      end case;

      Station_Info.Firmware_Version.Major := Partial_Version_Type (Confirmation (I + 2));
      Station_Info.Firmware_Version.Minor := Partial_Version_Type (Confirmation (I + 1));
      Station_Info.Firmware_Version.Build := Partial_Version_Type (Confirmation (I));

      I := I + 8;

      declare
         Flash_Model_Id : constant Flash_Model_Id_Type := Flash_Model_Id_Type (Confirmation (I)) + 256 * Flash_Model_Id_Type (Confirmation (I + 1)) +
                                                           65536 * Flash_Model_Id_Type (Confirmation (I + 2)) + 16777216 * Flash_Model_Id_Type (Confirmation (I + 3));
      begin
         case Flash_Model_Id is
            when 1            => Station_Info.Flash_Model := DEFAULT;
            when 16#00014015# => Station_Info.Flash_Model := S25FL216K;
            when 16#001c3114# => Station_Info.Flash_Model := EN25F80;
            when 16#00bf2541# => Station_Info.Flash_Model := SST25VF016B;
            when 16#00bf254a# => Station_Info.Flash_Model := SST25VF032B;
            when 16#00bf258e# => Station_Info.Flash_Model := SST25VF080B;
            when 16#00c22014# => Station_Info.Flash_Model := MX25L8006E;
            when 16#00c22015# => Station_Info.Flash_Model := MX25L1606E;
            when 16#00c22016# => Station_Info.Flash_Model := MX25L3206E;
            when 16#00c84014# => Station_Info.Flash_Model := GD25Q80B;
            when 16#00c84015# => Station_Info.Flash_Model := GD25Q16B;
            when 16#00c84016# => Station_Info.Flash_Model := GD25Q32B;
            when 16#00ef4014# => Station_Info.Flash_Model := W25Q80BV;
            when 16#00ef4015# => Station_Info.Flash_Model := W25Q16BV;
            when 16#00ef4016# => Station_Info.Flash_Model := W25Q32BV;
            when 16#00f83215# => Station_Info.Flash_Model := FM25S16;
            when others       => Station_Info.Flash_Model := UNKNOWN;
         end case;
      end;

      I := I + 4;

      case Confirmation (I) is
         when 0      => Station_Info.Homeplug_Version := V_1_1;
         when 1      => Station_Info.Homeplug_Version := V_2_0;
         when others => Station_Info.Homeplug_Version := Unknown;
      end case;

      I := I + 1;

      case Confirmation (I) is
         when 0      => Station_Info.Max_Bit_Rate := MBR_200;
         when 1      => Station_Info.Max_Bit_Rate := MBR_1000;
         when 2      => Station_Info.Max_Bit_Rate := MBR_1800;
         when others => Station_Info.Homeplug_Version := Unknown;
      end case;

      return Station_Info;

   end;

exception

   when Error : Packets.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Get_Station_Info;
