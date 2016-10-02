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
with HFID_String;
with Interfaces;
limited private with Interfaces.C;
with Simple;

package Ethernet.Datagram_Socket is

   subtype Milliseconds_Type is Natural;
   subtype Payload_Type      is Simple.Bytes_Type;

   type Protocol_Type is private;
   type Socket_Type   is tagged limited private;

   Message_No_Response         : constant String := "No response received from adapter";
   Message_Unexpected_Response : constant String := "Unexpected response received from adapter";

   Minimum_Payload_Size : constant := 46;
   Protocol_8912        : constant Protocol_Type;

   procedure Close(Socket : in out Socket_Type);

   procedure Open(Socket          : in out Socket_Type;
                  Protocol        : in     Protocol_Type;
                  Device_Name     : in     String;
                  Receive_Timeout : in     Milliseconds_Type := 0;
                  Send_Timeout    : in     Milliseconds_Type := 0);

   procedure Receive(Socket         : in     Socket_Type;
                     Payload        :    out Payload_Type;
                     Payload_Length :    out Natural;
                     From           :    out MAC_Address_Type);

   procedure Send(Socket  : in Socket_Type;
                  Payload : in Payload_Type;
                  To      : in MAC_Address_Type);

   function To_HFID_String(Payload : Payload_Type) return HFID_String.Bounded_String;

   Socket_Error : exception;

private

   use type Interfaces.C.int;

   type Protocol_Type is new Interfaces.C.unsigned_short;

   subtype Network_Protocol_Type is Protocol_Type;

   type Socket_Type is tagged limited
      record
         File_Descriptor  : Interfaces.C.int := -1;
         Interface_Index  : Interfaces.C.int := -1;
         Network_Protocol : Network_Protocol_Type;
      end record;

   Protocol_8912 : constant Protocol_Type := 16#8912#;

end Ethernet.Datagram_Socket;
