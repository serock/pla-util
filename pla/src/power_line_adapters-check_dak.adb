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

separate (Power_Line_Adapters)

function Check_DAK (Self                : Adapter_Type;
                    Pass_Phrase         : String;
                    Network_Device_Name : String) return Boolean is

   use type Octets.Octets_Type;

   Confirmation          : Packets.Payload_Type (1 .. Packets.Minimum_Payload_Size);
   Confirmation_Length   : Natural;
   DAK                   : Key_Type;
   Expected_Confirmation : Packets.Payload_Type (1 .. 12);
   Generated_DAK         : Key_Type;
   MAC_Address           : MAC_Addresses.MAC_Address_Type;
   Request_Payload       : Packets.Payload_Type (1 .. Packets.Minimum_Payload_Size);
   Socket                : Packet_Sockets.Thin.Socket_Type;

begin

   Validate_DAK_Pass_Phrase (Pass_Phrase      => Pass_Phrase,
                             Check_Min_Length => True);

   Request_Payload := (16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#09#, others => 16#00#);

   begin

      Socket.Open (Protocol        => Packet_Sockets.Thin.Protocol_8912,
                   Device_Name     => Network_Device_Name,
                   Receive_Timeout => Default_Receive_Timeout,
                   Send_Timeout    => Default_Send_Timeout);

      Self.Process (Request             => Request_Payload,
                    Socket              => Socket,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

      Expected_Confirmation := (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#02#, 16#04#, 16#01#, 16#00#);

      if Confirmation_Length < 16 or else Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

      DAK (1) := Confirmation (16);
      DAK (2) := Confirmation (15);
      DAK (3) := Confirmation (14);
      DAK (4) := Confirmation (13);

      Request_Payload := (16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#03#, 16#0a#, others => 16#00#);

      Self.Process (Request             => Request_Payload,
                    Socket              => Socket,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

      Expected_Confirmation := (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#03#, 16#04#, 16#01#, 16#00#);

      if Confirmation_Length < 16 or else Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

      DAK (5) := Confirmation (16);
      DAK (6) := Confirmation (15);
      DAK (7) := Confirmation (14);
      DAK (8) := Confirmation (13);

      Request_Payload := (16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#04#, 16#0b#, others => 16#00#);

      Self.Process (Request             => Request_Payload,
                    Socket              => Socket,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

      Expected_Confirmation := (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#04#, 16#04#, 16#01#, 16#00#);

      if Confirmation_Length < 16 or else Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

      DAK (9)  := Confirmation (16);
      DAK (10) := Confirmation (15);
      DAK (11) := Confirmation (14);
      DAK (12) := Confirmation (13);

      Request_Payload := (16#02#, 16#5c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#05#, 16#0c#, others => 16#00#);

      Self.Process (Request             => Request_Payload,
                    Socket              => Socket,
                    Confirmation        => Confirmation,
                    Confirmation_Length => Confirmation_Length,
                    From_MAC_Address    => MAC_Address);

      Expected_Confirmation := (16#02#, 16#5d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#05#, 16#04#, 16#01#, 16#00#);

      if Confirmation_Length < 16 or else Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

   exception

      when others =>
         Socket.Close;
         raise;

   end;

   Socket.Close;

   DAK (13) := Confirmation (16);
   DAK (14) := Confirmation (15);
   DAK (15) := Confirmation (14);
   DAK (16) := Confirmation (13);

   Generated_DAK := Generate_DAK (Pass_Phrase => Pass_Phrase);

   return Generated_DAK = DAK;

exception

   when Error : Packet_Sockets.Thin.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Check_DAK;
