------------------------------------------------------------------------
--  pla-util - A powerline adapter utility
--  Copyright (C) 2016-2022 John Serock
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
with Ada.Exceptions;
with Interfaces;
with Packet_Sockets.Thin;

use Interfaces;

separate (Power_Line_Adapter)

function Get_Network_Stats (Self                : Adapter_Type;
                            Network_Device_Name : String) return Network_Stats_List_Type is

   Expected_Response  : constant Packet_Sockets.Thin.Payload_Type := (16#02#, 16#49#, 16#60#, 16#00#, 16#00#);
   MAC_Address        : MAC_Address_Type;
   No_Stats           : Network_Stats_List_Type (1 .. 0);
   Number_Of_Stations : Natural;
   Request            : Packet_Sockets.Thin.Payload_Type (1 .. Packet_Sockets.Thin.Minimum_Payload_Size);
   Response           : Packet_Sockets.Thin.Payload_Type (1 .. 166);
   Response_Length    : Natural;
   Socket             : Packet_Sockets.Thin.Socket_Type;

begin

   Request := (16#02#, 16#48#, 16#60#, others => 16#00#);

   begin

      Socket.Open (Protocol        => Packet_Sockets.Thin.Protocol_HomePlug,
                   Device_Name     => Network_Device_Name,
                   Receive_Timeout => Default_Receive_Timeout,
                   Send_Timeout    => Default_Send_Timeout);

      Self.Process (Request          => Request,
                    Socket           => Socket,
                    Response         => Response,
                    Response_Length  => Response_Length,
                    From_MAC_Address => MAC_Address);

      if Response_Length < 6 or else Response (Expected_Response'Range) /= Expected_Response then
         raise Adapter_Error with Message_Unexpected_Response;
      end if;

   exception

      when others =>
         Socket.Close;
         raise;

   end;

   Socket.Close;

   Number_Of_Stations := Natural (Response (6));

   if Number_Of_Stations = 0 then
      return No_Stats;
   end if;

   declare

      Network_Stats : Network_Stats_List_Type (1 .. Number_Of_Stations);
      X             : Positive;

   begin

      X := 7;
      for I in 1 .. Number_Of_Stations loop

         Network_Stats (I).Destination_Address                    := Create_MAC_Address (Octets => Response (X .. X + 5));
         X := X + 6;
         Network_Stats (I).Average_PHY_Data_Rate_To_Destination   := Interfaces.Unsigned_16 (Response (X)) + 256 * Interfaces.Unsigned_16 (Response (X + 1)); X := X + 2;
         Network_Stats (I).Average_PHY_Data_Rate_From_Destination := Interfaces.Unsigned_16 (Response (X)) + 256 * Interfaces.Unsigned_16 (Response (X + 1)); X := X + 2;

      end loop;

      return Network_Stats;

   end;

exception

   when Error : Packet_Sockets.Thin.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Get_Network_Stats;
