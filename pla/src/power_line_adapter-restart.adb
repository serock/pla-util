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

procedure Restart (Self                : Adapter_Type;
                   Network_Device_Name : String) is

   Expected_Response : Packet_Sockets.Thin.Payload_Type (1 .. 10);
   MAC_Address       : MAC_Address_Type;
   Request           : Packet_Sockets.Thin.Payload_Type (1 .. Packet_Sockets.Thin.Minimum_Payload_Size);
   Response          : Packet_Sockets.Thin.Payload_Type (1 .. Packet_Sockets.Thin.Minimum_Payload_Size);
   Response_Length   : Natural;
   Socket            : Packet_Sockets.Thin.Socket_Type;

begin

   Request := (16#02#, 16#20#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#, 16#01#, others => 16#00#);

   Socket.Open (Protocol        => Packet_Sockets.Thin.Protocol_8912,
                Device_Name     => Network_Device_Name,
                Receive_Timeout => Default_Receive_Timeout,
                Send_Timeout    => Default_Send_Timeout);

   Self.Process (Request          => Request,
                 Socket           => Socket,
                 Response         => Response,
                 Response_Length  => Response_Length,
                 From_MAC_Address => MAC_Address);

   Expected_Response := (16#02#, 16#21#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#, 16#00#);

   if Response (Expected_Response'Range) /= Expected_Response then
      raise Packet_Sockets.Thin.Socket_Error with Packet_Sockets.Thin.Message_Unexpected_Response;
   end if;

   Socket.Close;

exception

   when others =>
      Socket.Close;
      raise;

end Restart;
