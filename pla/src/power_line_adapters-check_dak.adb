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

function Check_DAK (Self       : Adapter_Type;
                    Passphrase : String) return Boolean is

   use type Octets.Octets_Type;

   Confirmation          : Packets.Payload_Type (1 .. Packets.Minimum_Payload_Length);
   Confirmation_Length   : Natural;
   DAK                   : Octets.Key_Type;
   Expected_Confirmation : Packets.Payload_Type (1 .. 12);
   Generated_DAK         : Octets.Key_Type;
   MAC_Address           : MAC_Addresses.MAC_Address_Type;

begin

   Validate_DAK_Passphrase (Passphrase       => Passphrase,
                            Check_Min_Length => True);

   declare

      Request : constant Messages.Message_Type := Messages.Constructors.Create_Check_DAK_1_Request;

   begin

      Self.Process (Request             => Request,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

   end;

   Expected_Confirmation := (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#04#, 16#01#, 16#00#);

   if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
      raise Adapter_Error with Message_Unexpected_Confirmation;
   end if;

   DAK (1) := Confirmation (16);
   DAK (2) := Confirmation (15);
   DAK (3) := Confirmation (14);
   DAK (4) := Confirmation (13);

   declare

      Request : constant Messages.Message_Type := Messages.Constructors.Create_Check_DAK_2_Request;

   begin

      Self.Process (Request             => Request,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

   end;

   Expected_Confirmation := (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#03#, 16#04#, 16#01#, 16#00#);

   if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
      raise Adapter_Error with Message_Unexpected_Confirmation;
   end if;

   DAK (5) := Confirmation (16);
   DAK (6) := Confirmation (15);
   DAK (7) := Confirmation (14);
   DAK (8) := Confirmation (13);

   declare

      Request : constant Messages.Message_Type := Messages.Constructors.Create_Check_DAK_3_Request;

   begin

      Self.Process (Request             => Request,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

   end;

   Expected_Confirmation := (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#04#, 16#04#, 16#01#, 16#00#);

   if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
      raise Adapter_Error with Message_Unexpected_Confirmation;
   end if;

   DAK (9)  := Confirmation (16);
   DAK (10) := Confirmation (15);
   DAK (11) := Confirmation (14);
   DAK (12) := Confirmation (13);

   declare

      Request : constant Messages.Message_Type := Messages.Constructors.Create_Check_DAK_4_Request;

   begin

      Self.Process (Request             => Request,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

   end;

   Expected_Confirmation := (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#05#, 16#04#, 16#01#, 16#00#);

   if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
      raise Adapter_Error with Message_Unexpected_Confirmation;
   end if;

   DAK (13) := Confirmation (16);
   DAK (14) := Confirmation (15);
   DAK (15) := Confirmation (14);
   DAK (16) := Confirmation (13);

   Generated_DAK := Generate_DAK (Passphrase => Passphrase);

   return Generated_DAK = DAK;

exception

   when Error : Packets.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Check_DAK;
