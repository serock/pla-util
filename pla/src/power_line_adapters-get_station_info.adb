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

   Confirmation          : Packets.Payload_Type (1 .. 316);
   Confirmation_Length   : Natural;
   Expected_Confirmation : constant Packets.Payload_Type := (16#02#, 16#4d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#);
   MAC_Address           : MAC_Addresses.MAC_Address_Type;

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

      Chip_Version_Id : constant Chip_Version_Id_Type := Chip_Version_Id_Type (Confirmation (10)) + 256 * Chip_Version_Id_Type (Confirmation (11)) +
                                                         65536 * Chip_Version_Id_Type (Confirmation (12)) + 16777216 * Chip_Version_Id_Type (Confirmation (13));
      Station_Info    : Station_Info_Type;

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

      return Station_Info;

   end;

exception

   when Error : Packets.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Get_Station_Info;
