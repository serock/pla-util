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

use type Ethernet.Datagram_Socket.Payload_Type;

separate (Power_Line_Adapter)

function Get_Manufacturer_HFID(Adapter : in Adapter_Type;
                               Socket  : in Ethernet.Datagram_Socket.Socket_Type) return HFID_String.Bounded_String is

   Expected_Response : constant Ethernet.Datagram_Socket.Payload_Type :=
     (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#01#, 16#40#, 16#00#);

   MAC_Address     : Ethernet.MAC_Address_Type;
   Request         : Ethernet.Datagram_Socket.Payload_Type(1 .. Ethernet.Datagram_Socket.Minimum_Payload_Size);
   Response        : Ethernet.Datagram_Socket.Payload_Type(1 .. 76);
   Response_Length : Natural;

begin

   Request := (16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#,
               16#02#, 16#1b#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
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

      return Ethernet.Datagram_Socket.To_HFID_String(Payload => Response(13 .. Response_Length));

   else

      raise Ethernet.Datagram_Socket.Socket_Error with Ethernet.Datagram_Socket.Message_Unexpected_Response;

   end if;

end Get_Manufacturer_HFID;
