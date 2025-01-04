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
with Ada.Exceptions;
with Messages.Constructors;

separate (Power_Line_Adapters)

function Get_Capabilities (Self : Adapter_Type) return Capabilities_Type is

   use type Octets.Octets_Type;

   Capabilities          : Capabilities_Type;
   Confirmation          : Packets.Payload_Type (1 .. Packets.Minimum_Payload_Length);
   Confirmation_Length   : Natural;
   Expected_Confirmation : constant Packets.Payload_Type := (16#01#, 16#35#, 16#60#, 16#00#, 16#00#);
   MAC_Address           : MAC_Addresses.MAC_Address_Type;

begin

   declare

      Request : constant Messages.Message_Type := Messages.Constructors.Create_Get_Capabilities_Request;

   begin

      Self.Process (Request             => Request,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

   end;

   if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
      raise Adapter_Error with Message_Unexpected_Confirmation;
   end if;

   Capabilities.AV_Version             := AV_Version_Type'Val (Confirmation (6));
   Capabilities.MAC_Address            := MAC_Addresses.Create_MAC_Address (MAC_Address_Octets => Confirmation (7 .. 12));
   Capabilities.OUI                    := 65536 * OUI_Type (Confirmation (13)) + 256 * OUI_Type (Confirmation (14)) + OUI_Type (Confirmation (15));
   Capabilities.Proxy                  := Capable_Type'Val (Confirmation (19));
   Capabilities.Backup_CCo             := Capable_Type'Val (Confirmation (20));
   Capabilities.Implementation_Version := Implementation_Version_Type (Confirmation (29)) + 256 * Implementation_Version_Type (Confirmation (30));

   return Capabilities;

exception

   when Error : Packets.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Get_Capabilities;
