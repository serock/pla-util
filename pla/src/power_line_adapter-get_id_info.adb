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

separate (Power_Line_Adapter)

function Get_Id_Info (Self                : Adapter_Type;
                      Network_Device_Name : String) return Id_Info_Type is

   Expected_Response : constant Packet_Sockets.Thin.Payload_Type := (16#01#, 16#61#, 16#60#, 16#00#, 16#00#);
   Id_Info           : Id_Info_Type;
   MAC_Address       : MAC_Address_Type;
   Request           : Packet_Sockets.Thin.Payload_Type (1 .. Packet_Sockets.Thin.Minimum_Payload_Size);
   Response          : Packet_Sockets.Thin.Payload_Type (1 .. 266);
   Response_Length   : Natural;
   Socket            : Packet_Sockets.Thin.Socket_Type;

begin

   Request := (16#01#, 16#60#, 16#60#, others => 16#00#);

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

      if Response_Length < 11 or else Response (Expected_Response'Range) /= Expected_Response then
         raise Adapter_Error with Message_Unexpected_Response;
      end if;

   exception

      when others =>
         Socket.Close;
         raise;

   end;

   Socket.Close;

   case Response (10) is
      when 16#00# => Id_Info.Homeplug_AV_Version := HPAV_1_1;
      when 16#01# => Id_Info.Homeplug_AV_Version := HPAV_2_0;
      when 16#ff# => Id_Info.Homeplug_AV_Version := Not_HPAV;
      when others =>
         raise Adapter_Error with Message_Unexpected_Response;
   end case;

   if Id_Info.Homeplug_AV_Version = HPAV_2_0 then

      Id_Info.MCS := MCS_Type'Val (Response (12));

   else

      Id_Info.MCS := MIMO_Not_Supported;

   end if;

   return Id_Info;

exception

   when Error : Packet_Sockets.Thin.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Get_Id_Info;
