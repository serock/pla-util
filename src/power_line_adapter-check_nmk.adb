------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016 John Serock
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
with Ethernet.Datagram_Socket;
with Interfaces;

use type Ethernet.Datagram_Socket.Payload_Type;
use type Interfaces.Unsigned_8;

separate (Power_Line_Adapter)

function Check_NMK(Adapter     : in Adapter_Type;
                   Pass_Phrase : in String;
                   Socket      : in Ethernet.Datagram_Socket.Socket_Type) return Boolean is

   Expected_Response : Ethernet.Datagram_Socket.Payload_Type(1 .. 12);
   Generated_NMK     : Key_Type;
   I                 : Positive := 13;
   MAC_Address       : Ethernet.MAC_Address_Type;
   NMK               : Key_Type;
   Request           : Ethernet.Datagram_Socket.Payload_Type(1 .. Ethernet.Datagram_Socket.Minimum_Payload_Size);
   Response          : Ethernet.Datagram_Socket.Payload_Type(1 .. Ethernet.Datagram_Socket.Minimum_Payload_Size);
   Response_Length   : Natural;

begin

   Validate_NMK_Pass_Phrase(Pass_Phrase      => Pass_Phrase,
                            Check_Min_Length => False);

   Request := (16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#24#, others => 16#00#);

   Adapter.Process(Request          => Request,
                   Socket           => Socket,
                   Response         => Response,
                   Response_Length  => Response_Length,
                   From_MAC_Address => MAC_Address);

   Expected_Response := (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#01#, 16#10#, 16#00#);

   if Response_Length < 28 or else Response(Expected_Response'Range) /= Expected_Response then

      raise Ethernet.Datagram_Socket.Socket_Error with Ethernet.Datagram_Socket.Message_Unexpected_Response;

   end if;

   NMK := Response(I .. I + Key_Type'Length - 1);

   Generated_NMK := Generate_NMK(Pass_Phrase => Pass_Phrase);

   return Generated_NMK = NMK;

end Check_NMK;
