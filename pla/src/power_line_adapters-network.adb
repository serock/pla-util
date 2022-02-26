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
with Packet_Sockets.Thin;

use type Packet_Sockets.Thin.Payload_Type;

package body Power_Line_Adapters.Network is

   function Discover (Network_Device_Name : String;
                      MAC_Address         : MAC_Address_Type := Broadcast_MAC_Address) return Power_Line_Adapter_Sets.Set is

      Adapter           : Adapter_Type;
      Adapters          : Power_Line_Adapter_Sets.Set (Capacity => Max_Adapters);
      Expected_Response : constant Packet_Sockets.Thin.Payload_Type := (16#02#, 16#71#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#);
      Network_Interface : Network_Interface_Type;
      PLA_MAC_Address   : MAC_Address_Type;
      Request           : Packet_Sockets.Thin.Payload_Type (1 .. Packet_Sockets.Thin.Minimum_Payload_Size);
      Response          : Packet_Sockets.Thin.Payload_Type (1 .. 75);
      Response_Length   : Natural;
      Socket            : Packet_Sockets.Thin.Socket_Type;

   begin

      Request := (16#01#, 16#70#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#, 16#a3#, 16#97#, 16#a2#, 16#55#, 16#53#, 16#be#, 16#f1#,
                  16#fc#, 16#f9#, 16#79#, 16#6b#, 16#52#, 16#14#, 16#13#, 16#e9#, 16#e2#, others => 16#00#);

      begin

         Socket.Open (Protocol        => Packet_Sockets.Thin.Protocol_8912,
                      Device_Name     => Network_Device_Name,
                      Receive_Timeout => Default_Receive_Timeout,
                      Send_Timeout    => Default_Send_Timeout);

         Socket.Send (Payload => Request,
                      To      => MAC_Address);

         loop
            Socket.Receive (Payload        => Response,
                            Payload_Length => Response_Length,
                            From           => PLA_MAC_Address);

            if Response_Length = 0 then
               exit;
            end if;

            if Response (Expected_Response'Range) /= Expected_Response then
               raise Adapter_Error with Message_Unexpected_Response;
            end if;

            case Response (10) is
               when 0      => Network_Interface := MII0;
               when 1      => Network_Interface := MII1;
               when 2 | 3  => Network_Interface := PLC;
               when 4      => Network_Interface := SDR;
               when others =>
                  raise Adapter_Error with Message_Unexpected_Response;
            end case;

            Adapter.Create (Network_Interface => Network_Interface,
                            MAC_Address       => PLA_MAC_Address,
                            HFID              => Packet_Sockets.Thin.To_HFID_String (Payload => Response (12 .. Response_Length)));

            Adapters.Include (New_Item => Adapter);
         end loop;

      exception

         when others =>
            Socket.Close;
            raise;

      end;

      Socket.Close;

      return Adapters;

   exception

      when Error : Packet_Sockets.Thin.Packet_Error =>
         raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

   end Discover;

end Power_Line_Adapters.Network;
