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

function Get_Discover_List (Self : Adapter_Type) return Discover_List_Type is

   use type Octets.Octets_Type;

   Confirmation          : Packets.Payload_Type (1 .. 385);
   Confirmation_Length   : Natural;
   Expected_Confirmation : constant Packets.Payload_Type := (16#01#, 16#15#, 16#00#, 16#00#, 16#00#);
   MAC_Address           : MAC_Addresses.MAC_Address_Type;
   Number_Of_Networks    : Network_Count_Type;
   Number_Of_Stations    : Station_Count_Type;
   Octets_Per_Network    : constant := 13;
   Octets_Per_Station    : constant := 12;
   X                     : Positive;

begin

   declare

      Request : constant Messages.Message_Type := Messages.Constructors.Create_Get_Discover_List_Request;

   begin

      Self.Process (Request             => Request,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

   end;

   if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
      raise Adapter_Error with Message_Unexpected_Confirmation;
   end if;

   X                  := 6;
   Number_Of_Stations := Natural (Confirmation (X));
   X                  := X + 1;
   Number_Of_Networks := Natural (Confirmation (X + Number_Of_Stations * Octets_Per_Station));

   declare

      use type Octets.Octet_Type;

      Discovered_List : Discover_List_Type (Number_Of_Stations => Number_Of_Stations,
                                            Number_Of_Networks => Number_Of_Networks);
      NID             : NID_Type;

   begin

      for I in 1 .. Number_Of_Stations loop

         Discovered_List.Stations (I).MAC_Address  := MAC_Addresses.Create_MAC_Address (MAC_Address_Octets => Confirmation (X .. X + 5));
         Discovered_List.Stations (I).TEI          := TEI_Type (Confirmation (X + 6));
         Discovered_List.Stations (I).Same_Network := No_Yes_Type'Val (Confirmation (X + 7));
         Discovered_List.Stations (I).SNID         := SNID_Type (Confirmation (X + 8) and 16#0f#);
         Discovered_List.Stations (I).CCo          := (if (Confirmation (X + 9) and 16#20#) = 0 then No else Yes);
         Discovered_List.Stations (I).PCo          := (if (Confirmation (X + 9) and 16#40#) = 0 then No else Yes);
         Discovered_List.Stations (I).Backup_CCo   := (if (Confirmation (X + 9) and 16#80#) = 0 then No else Yes);
         Discovered_List.Stations (I).Signal_Level := Signal_Level_Type (Confirmation (X + 10));
         X                                         := X + Octets_Per_Station;

      end loop;

      X := X + 1;

      for I in 1 .. Number_Of_Networks loop

         NID := NID_Type (Confirmation (X));
         NID := NID + NID_Type (Confirmation (X + 1)) * 16#00_0000_0000_0100#;
         NID := NID + NID_Type (Confirmation (X + 2)) * 16#00_0000_0001_0000#;
         NID := NID + NID_Type (Confirmation (X + 3)) * 16#00_0000_0100_0000#;
         NID := NID + NID_Type (Confirmation (X + 4)) * 16#00_0001_0000_0000#;
         NID := NID + NID_Type (Confirmation (X + 5)) * 16#00_0100_0000_0000#;
         NID := NID + NID_Type (Confirmation (X + 6)) * 16#01_0000_0000_0000#;

         Discovered_List.Networks (I).NID                 := NID;
         Discovered_List.Networks (I).SNID                := SNID_Type (Confirmation (X + 7) and 16#0f#);
         Discovered_List.Networks (I).Coordinating_Status := Coordinating_Status_Type'Val (Confirmation (X + 10));
         X                                                := X + Octets_Per_Network;

      end loop;

      return Discovered_List;

   end;

exception

   when Error : Packets.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Get_Discover_List;
