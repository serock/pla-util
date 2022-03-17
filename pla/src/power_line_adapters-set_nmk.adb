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
with Messages.Constructors;
with Packet_Sockets.Thin;

separate (Power_Line_Adapters)

procedure Set_NMK (Self                : Adapter_Type;
                   Passphrase          : String;
                   Network_Device_Name : String) is

   Confirmation          : Packets.Payload_Type (1 .. Packets.Minimum_Payload_Length);
   Confirmation_Length   : Natural;
   Expected_Confirmation : constant Packets.Payload_Type := (16#02#, 16#19#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#, 16#00#);
   MAC_Address           : MAC_Addresses.MAC_Address_Type;
   Socket                : Packet_Sockets.Thin.Socket_Type;

begin

   Validate_NMK_Passphrase (Passphrase       => Passphrase,
                            Check_Min_Length => True);

   --  Request_Payload (1 .. 9)                     := (16#02#, 16#18#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#);
   --  Request_Payload (10 .. 25)                   := Generate_NMK (Passphrase => Passphrase);
   --  Request_Payload (26 .. 27)                   := (16#00#, 16#01#);
   --  Request_Payload (28 .. Request_Payload'Last) := (others => 16#00#);

   declare

      use type Octets.Octets_Type;

   begin

      Socket.Open (Protocol        => Packet_Sockets.Thin.Protocol_8912,
                   Device_Name     => Network_Device_Name,
                   Receive_Timeout => Default_Receive_Timeout,
                   Send_Timeout    => Default_Send_Timeout);

      declare

         Request : constant Messages.Message_Type := Messages.Constructors.Create_Set_NMK_Request (Key => Generate_NMK (Passphrase => Passphrase));

      begin

         Self.Process (Request             => Request,
                       Socket              => Socket,
                       Confirmation        => Confirmation,
                       Confirmation_Length => Confirmation_Length,
                       From_MAC_Address    => MAC_Address);

      end;

      if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

   exception

      when others =>
         Socket.Close;
         raise;

   end;

   Socket.Close;

exception

   when Error : Packets.Packet_Error | Packet_Sockets.Thin.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Set_NMK;
