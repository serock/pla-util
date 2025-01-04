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

function Get_Network_Info (Self  : Adapter_Type;
                           Scope : Network_Scope_Type) return Network_Info_List_Type is

   Confirmation          : Packets.Payload_Type (1 .. 385);
   Confirmation_Length   : Natural;
   Expected_Confirmation : constant Packets.Payload_Type := (16#02#, 16#29#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#);
   MAC_Address           : MAC_Addresses.MAC_Address_Type;
   No_Network            : Network_Info_List_Type (1 .. 0);
   Number_Of_Networks    : Natural;

begin

   declare

      use type Octets.Octets_Type;

      Request : constant Messages.Message_Type := (if Scope = ANY then Messages.Constructors.Create_Get_Any_Network_Info_Request else Messages.Constructors.Create_Get_Member_Network_Info_Request);

   begin

      Self.Process (Request             => Request,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

      if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

   end;

   Number_Of_Networks := Natural (Confirmation (10));

   if Number_Of_Networks = 0 then
      return No_Network;
   end if;

   declare

      use type Octets.Octet_Type;

      Network_Info : Network_Info_List_Type (1 .. Number_Of_Networks);
      NID          : NID_Type;
      X            : Positive := 11;

   begin

      for I in 1 .. Number_Of_Networks loop

         NID := NID_Type (Confirmation (X));
         NID := NID + NID_Type (Confirmation (X + 1)) * 16#00_0000_0000_0100#;
         NID := NID + NID_Type (Confirmation (X + 2)) * 16#00_0000_0001_0000#;
         NID := NID + NID_Type (Confirmation (X + 3)) * 16#00_0000_0100_0000#;
         NID := NID + NID_Type (Confirmation (X + 4)) * 16#00_0001_0000_0000#;
         NID := NID + NID_Type (Confirmation (X + 5)) * 16#00_0100_0000_0000#;
         NID := NID + NID_Type (Confirmation (X + 6)) * 16#01_0000_0000_0000#;

         Network_Info (I).NID := NID;
         Network_Info (I).SNID               := SNID_Type (Confirmation (X + 7) and 16#0f#);
         Network_Info (I).TEI                := TEI_Type (Confirmation (X + 8));
         Network_Info (I).Station_Role       := Station_Role_Type'Val (Confirmation (X + 9));
         Network_Info (I).CCo_MAC_Address    := MAC_Addresses.Create_MAC_Address (MAC_Address_Octets => Confirmation (X + 10 .. X + 15));
         Network_Info (I).Network_Kind       := Network_Kind_Type'Val (Confirmation (X + 16));
         Network_Info (I).Num_Coord_Networks := Network_Count_Type (Confirmation (X + 17));
         Network_Info (I).Status             := Status_Type'Val (Confirmation (X + 18));

         X := X + 19;

      end loop;

      for I in 1 .. Number_Of_Networks loop

         Network_Info (I).BCCo_MAC_Address := MAC_Addresses.Create_MAC_Address (MAC_Address_Octets => Confirmation (X .. X + 5));

         X := X + 6;

      end loop;

      return Network_Info;

   end;

exception

   when Error : Packets.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Get_Network_Info;
