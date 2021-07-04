------------------------------------------------------------------------
--  pla-util - A powerline adapter utility
--  Copyright (C) 2016-2021 John Serock
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program. If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------
with Packet_Sockets.Thin;

use type Packet_Sockets.Thin.Payload_Type;

separate (Power_Line_Adapter)

function Get_Network_Info (Self   : Adapter_Type;
                           Arg    : Interfaces.Unsigned_8;
                           Socket : Packet_Sockets.Thin.Socket_Type) return Network_Info_List_Type is

   Expected_Response  : Packet_Sockets.Thin.Payload_Type (1 .. 9);
   MAC_Address        : MAC_Address_Type;
   No_Network         : Network_Info_List_Type (1 .. 0);
   Number_Of_Networks : Natural;
   Request            : Packet_Sockets.Thin.Payload_Type (1 .. Packet_Sockets.Thin.Minimum_Payload_Size);
   Response           : Packet_Sockets.Thin.Payload_Type (1 .. 385);
   Response_Length    : Natural;

begin

   Request := (16#02#, 16#28#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#, 16#00#, Arg, others => 16#00#);

   Self.Process (Request          => Request,
                 Socket           => Socket,
                 Response         => Response,
                 Response_Length  => Response_Length,
                 From_MAC_Address => MAC_Address);

   Expected_Response := (16#02#, 16#29#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#);

   if Response_Length < 26 or else Response (Expected_Response'Range) /= Expected_Response then
      raise Packet_Sockets.Thin.Socket_Error with Packet_Sockets.Thin.Message_Unexpected_Response;
   end if;

   Number_Of_Networks := Natural (Response (10));

   if Number_Of_Networks = 0 then
      return No_Network;
   end if;

   declare

      Network_Info : Network_Info_List_Type (1 .. Number_Of_Networks);
      X            : Positive;

   begin

      X := 11;
      for I in 1 .. Number_Of_Networks loop
         Network_Info (I).NID := Response (X .. X + NID_Type'Length - 1);
         X := X + NID_Type'Length;
         Network_Info (I).SNID            := Response (X);                         X := X + 1;
         Network_Info (I).TEI             := Response (X);                         X := X + 1;
         Network_Info (I).Station_Role    := Station_Role_Type'Val (Response (X)); X := X + 1;
         Network_Info (I).CCo_MAC_Address := Create_MAC_Address (Octets => Response (X .. X + 5));
         X := X + 6;
         Network_Info (I).Network_Kind       := Network_Kind_Type'Val (Response (X)); X := X + 1;
         Network_Info (I).Num_Coord_Networks := Response (X);                         X := X + 1;
         Network_Info (I).Status             := Status_Type'Val (Response (X));       X := X + 1;
      end loop;

      for I in 1 .. Number_Of_Networks loop
         Network_Info (I).BCCo_MAC_Address := Create_MAC_Address (Octets => Response (X .. X + 5));
         X := X + 6;
      end loop;

      return Network_Info;

   end;

end Get_Network_Info;
