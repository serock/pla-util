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

function Get_Network_Stats (Self                : Adapter_Type;
                            Network_Device_Name : String) return Network_Stats_List_Type is

   Confirmation          : Packets.Payload_Type (1 .. 170);
   Confirmation_Length   : Natural;
   Expected_Confirmation : constant Packets.Payload_Type := (16#02#, 16#2d#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#);
   MAC_Address           : MAC_Addresses.MAC_Address_Type;
   No_Stats              : Network_Stats_List_Type (1 .. 0);
   Number_Of_Stations    : Natural;
   Request_Payload       : Packets.Payload_Type (1 .. Packets.Minimum_Payload_Size);
   Socket                : Packet_Sockets.Thin.Socket_Type;

begin

   Request_Payload := (16#02#, 16#2c#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#, 16#00#, 16#b0#, 16#f2#, 16#e6#, 16#95#, 16#66#, 16#6b#, 16#03#, others => 16#00#);

   declare

      use type Octets.Octets_Type;

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

      if Confirmation_Length < 10 or else Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

   exception

      when others =>
         Socket.Close;
         raise;

   end;

   Socket.Close;

   Number_Of_Stations := Natural (Confirmation (10));

   if Number_Of_Stations = 0 then
      return No_Stats;
   end if;

   declare

      use type Octets.Octet_Type;

      Network_Stats : Network_Stats_List_Type (1 .. Number_Of_Stations);
      X             : Positive;

   begin

      X := 11;
      for I in 1 .. Number_Of_Stations loop

         Network_Stats (I).Destination_Address    := MAC_Addresses.Create_MAC_Address (Octets => Confirmation (X .. X + 5));
         X := X + 6;
         Network_Stats (I).Average_Rate_To_Dest   := Data_Rate_Type (Confirmation (X)) + 256 * (Data_Rate_Type (Confirmation (X + 1) and 16#07#));
         X := X + 2;
         Network_Stats (I).Average_Rate_From_Dest := Data_Rate_Type (Confirmation (X)) + 256 * (Data_Rate_Type (Confirmation (X + 1) and 16#07#));
         X := X + 2;

      end loop;

      return Network_Stats;

   end;

exception

   when Error : Packet_Sockets.Thin.Packet_Error =>
      raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

end Get_Network_Stats;
