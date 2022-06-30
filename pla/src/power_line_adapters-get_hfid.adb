--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016-2022 John Serock
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

function Get_HFID (Self                : Adapter_Type;
                   Kind                : HFID_Kind_Type;
                   Network_Device_Name : String) return HFID_Strings.Bounded_String is

   use type Octets.Octets_Type;

   Confirmation          : Packets.Payload_Type (1 .. 76);
   Confirmation_Length   : Natural;
   Expected_Confirmation : constant Packets.Payload_Type := (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#01#, 16#40#, 16#00#);
   MAC_Address           : MAC_Addresses.MAC_Address_Type;

begin

      declare

         Request : constant Messages.Message_Type := (if Kind = MANUFACTURER then Messages.Constructors.Create_Get_Manufacturer_HFID_Request else Messages.Constructors.Create_Get_User_HFID_Request);

      begin

         Self.Process (Request             => Request,
                       Confirmation        => Confirmation,
                       Confirmation_Length => Confirmation_Length,
                       From_MAC_Address    => MAC_Address);

      end;

      if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

   return To_HFID_String (HFID_Octets => Confirmation (13 .. Confirmation_Length));

exception

   when Error : Packets.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Get_HFID;