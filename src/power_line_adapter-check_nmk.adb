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

   Expected_Response : constant Ethernet.Datagram_Socket.Payload_Type :=
     (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#01#, 16#10#, 16#00#);

   Is_Match        : Boolean  := True;
   J               : Positive := 13;
   MAC_Address     : Ethernet.MAC_Address_Type;
   NMK             : Network_Membership_Key_Type;
   Request         : Ethernet.Datagram_Socket.Payload_Type(1 .. Ethernet.Datagram_Socket.Minimum_Payload_Size);
   Response        : Ethernet.Datagram_Socket.Payload_Type(1 .. Ethernet.Datagram_Socket.Minimum_Payload_Size);
   Response_Length : Natural;

begin

   Validate_Pass_Phrase(Pass_Phrase      => Pass_Phrase,
                        Check_Min_Length => False);

   Request := (16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#,
               16#02#, 16#24#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
               16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
               16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
               16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
               16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#);

   Socket.Send(Payload => Request,
               To      => Adapter.Get_MAC_Address);

   Socket.Receive(Payload        => Response,
                  Payload_Length => Response_Length,
                  From           => MAC_Address);

   if Response_Length = 0 then

      raise Ethernet.Datagram_Socket.Socket_Error with Ethernet.Datagram_Socket.Message_No_Response;

   elsif Response(Expected_Response'Range) = Expected_Response then

      NMK := Generate_NMK(Pass_Phrase => Pass_Phrase);

      for I in NMK'Range loop

         if NMK(I) /= Response(J) then

            Is_Match := False;

            exit;

         end if;

         J := J + 1;

      end loop;

      return Is_Match;

   else

      raise Ethernet.Datagram_Socket.Socket_Error with Ethernet.Datagram_Socket.Message_Unexpected_Response;

   end if;

end Check_NMK;
