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

function Get_Network_Stats (Self                : Adapter_Type;
                            Network_Device_Name : String) return Network_Stats_List_Type is

   Confirmation          : Packets.Payload_Type (1 .. 170);
   Confirmation_Length   : Natural;
   Expected_Confirmation : constant Packets.Payload_Type := (16#02#, 16#2d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#);
   MAC_Address           : MAC_Addresses.MAC_Address_Type;
   No_Stats              : Network_Stats_List_Type (1 .. 0);
   Number_Of_Stations    : Natural;

begin

   declare

      use type Octets.Octets_Type;

      Request : constant Messages.Message_Type := Messages.Constructors.Create_Get_Network_Stats_Request;

   begin

      Self.Process (Request             => Request,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

      if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

   end;

   Number_Of_Stations := Natural (Confirmation (10));

   if Number_Of_Stations = 0 then
      return No_Stats;
   end if;

   declare

      use type Octets.Octet_Type;

      Network_Stats : Network_Stats_List_Type (1 .. Number_Of_Stations);
      X             : Positive := 11;

   begin

      for I in 1 .. Number_Of_Stations loop

         Network_Stats (I).Destination_Address    := MAC_Addresses.Create_MAC_Address (Octets => Confirmation (X .. X + 5));
         Network_Stats (I).Average_Rate_To_Dest   := Data_Rate_Type (Confirmation (X + 6)) + 256 * (Data_Rate_Type (Confirmation (X + 7) and 16#07#));
         Network_Stats (I).Average_Rate_From_Dest := Data_Rate_Type (Confirmation (X + 8)) + 256 * (Data_Rate_Type (Confirmation (X + 9) and 16#07#));

         X := X + 10;

      end loop;

      return Network_Stats;

   end;

exception

   when Error : Packets.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Get_Network_Stats;
