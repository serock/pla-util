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
with HFID_String;
with Interfaces;
private with Interfaces.C;
with MAC_Addresses;
with Octets;

use MAC_Addresses;
use Octets;

package Packet_Sockets.Thin is

   type Protocol_Type is private;
   type Socket_Type   is tagged limited private;

   subtype Milliseconds_Type is Natural;
   subtype Payload_Type      is Octets_Type;

   Protocol_8912        : constant Protocol_Type;
   Protocol_HomePlug    : constant Protocol_Type;
   Minimum_Payload_Size : constant := 46;

   Packet_Error : exception;

   procedure Close (Self : in out Socket_Type);

   procedure Open (Socket          : in out Socket_Type;
                   Protocol        :        Protocol_Type;
                   Device_Name     :        String;
                   Receive_Timeout :        Milliseconds_Type := 0;
                   Send_Timeout    :        Milliseconds_Type := 0);

   procedure Receive (Self           :     Socket_Type;
                      Payload        : out Payload_Type;
                      Payload_Length : out Natural;
                      From           : out MAC_Address_Type);

   procedure Send (Self    : Socket_Type;
                   Payload : Payload_Type;
                   To      : MAC_Address_Type);

   function To_HFID_String (Payload : Payload_Type) return HFID_String.Bounded_String;

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

   Protocol_8912     : constant Protocol_Type := 16#8912#;
   Protocol_HomePlug : constant Protocol_Type := 16#88e1#;

   procedure Bind (File_Descriptor  : Interfaces.C.int;
                   Network_Protocol : Network_Protocol_Type;
                   Interface_Index  : Interfaces.C.int);

   procedure Close_Quietly (File_Descriptor : in out Interfaces.C.int);

   function Create_Socket (Network_Protocol : Network_Protocol_Type) return Interfaces.C.int;

   function Errno return Interfaces.C.int;

   function Error_Message (Error_Number : Interfaces.C.int) return String;

   function Get_Interface_Index (Device_Name : String;
                                 Fd          : Interfaces.C.int) return Interfaces.C.int;

   function Is_Open (Self : Socket_Type) return Boolean;

   procedure Set_Socket_Timeout_Option (File_Descriptor : Interfaces.C.int;
                                        Option_Name     : Interfaces.C.int;
                                        Timeout         : Milliseconds_Type);

end Packet_Sockets.Thin;
