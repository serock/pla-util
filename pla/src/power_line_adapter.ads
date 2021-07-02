------------------------------------------------------------------------
--  pla-util - A powerline adapter utility
--  Copyright (C) 2016-2021 John Serock
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
with Packet_Sockets.Thin;
private with Ada.Streams;

package Power_Line_Adapter is

   subtype NID_Type is Packet_Sockets.Thin.Bytes_Type (1 .. 7);

   type Adapter_Type is tagged private;

   type Network_Kind_Type is (IN_HOME_NETWORK, ACCESS_NETWORK);
   type Station_Role_Type is (UNASSOC_STA, UNASSOC_CCO, STA, CCO, BACKUP_CCO);
   type Status_Type       is (JOINED, NOT_JOINED_HAVE_NMK, NOT_JOINED_NO_NMK);

   type Network_Info_Type is
      record
         NID                : NID_Type;
         SNID               : Interfaces.Unsigned_8;
         TEI                : Interfaces.Unsigned_8;
         CCo_MAC_Address    : Packet_Sockets.Thin.MAC_Address_Type;
         BCCo_MAC_Address   : Packet_Sockets.Thin.MAC_Address_Type;
         Num_Coord_Networks : Interfaces.Unsigned_8;
         Station_Role       : Station_Role_Type;
         Network_Kind       : Network_Kind_Type;
         Status             : Status_Type;
      end record;

   type Network_Info_List_Type is array (Positive range <>) of Network_Info_Type;

   Max_Adapters : constant := 16;

   Input_Error : exception;

   function "<" (Left  : Adapter_Type;
                 Right : Adapter_Type) return Boolean;

   overriding function "=" (Left  : Adapter_Type;
                            Right : Adapter_Type) return Boolean;

   function Check_DAK (Adapter     : Adapter_Type;
                       Pass_Phrase : String;
                       Socket      : Packet_Sockets.Thin.Socket_Type) return Boolean;

   function Check_NMK (Adapter     : Adapter_Type;
                       Pass_Phrase : String;
                       Socket      : Packet_Sockets.Thin.Socket_Type) return Boolean;

   function Get_MAC_Address (Adapter : Adapter_Type) return Packet_Sockets.Thin.MAC_Address_Type;

   function Has_MAC_Address (Adapter     : Adapter_Type;
                             MAC_Address : String) return Boolean;

   function Get_Manufacturer_HFID (Adapter : Adapter_Type;
                                   Socket  : Packet_Sockets.Thin.Socket_Type) return HFID_String.Bounded_String;

   function Get_User_HFID (Adapter : Adapter_Type;
                           Socket  : Packet_Sockets.Thin.Socket_Type) return HFID_String.Bounded_String;

   function Get_Any_Network_Info (Adapter : Adapter_Type;
                                  Socket  : Packet_Sockets.Thin.Socket_Type) return Network_Info_List_Type;

   function Get_Member_Network_Info (Adapter : Adapter_Type;
                                     Socket  : Packet_Sockets.Thin.Socket_Type) return Network_Info_List_Type;

   procedure Reset (Adapter : Adapter_Type;
                    Socket  : Packet_Sockets.Thin.Socket_Type);

   procedure Restart (Adapter : Adapter_Type;
                      Socket  : Packet_Sockets.Thin.Socket_Type);

   procedure Set_HFID (Adapter : Adapter_Type;
                       HFID    : HFID_String.Bounded_String;
                       Socket  : Packet_Sockets.Thin.Socket_Type);

   procedure Set_NMK (Adapter     : Adapter_Type;
                      Pass_Phrase : String;
                      Socket      : Packet_Sockets.Thin.Socket_Type);

   function To_String (Adapter : Adapter_Type) return String;

private

   type Adapter_Type is tagged
      record
         Network_Interface : Natural;
         MAC_Address       : Packet_Sockets.Thin.MAC_Address_Type;
         HFID              : HFID_String.Bounded_String;
      end record;

   subtype HFID_Bytes_Type is Packet_Sockets.Thin.Bytes_Type (1 .. 64);
   subtype Key_Type        is Packet_Sockets.Thin.Bytes_Type (1 .. 16);

   procedure Create (Adapter           : in out Adapter_Type;
                     Network_Interface :        Natural;
                     MAC_Address       :        Packet_Sockets.Thin.MAC_Address_Type;
                     HFID              :        HFID_String.Bounded_String);

   function Generate_DAK (Pass_Phrase : String) return Key_Type;

   function Generate_Key (Pass_Phrase : String;
                          Salt        : Ada.Streams.Stream_Element_Array) return Key_Type;

   function Generate_NMK (Pass_Phrase : String) return Key_Type;

   function Get_Bytes (HFID : HFID_String.Bounded_String) return HFID_Bytes_Type;

   function Get_HFID (Arg     : Interfaces.Unsigned_8;
                      Adapter : Adapter_Type;
                      Socket  : Packet_Sockets.Thin.Socket_Type) return HFID_String.Bounded_String;

   function Get_Network_Info (Arg     : Interfaces.Unsigned_8;
                              Adapter : Adapter_Type;
                              Socket  : Packet_Sockets.Thin.Socket_Type) return Network_Info_List_Type;

   procedure Process (Adapter          :     Adapter_Type;
                      Request          :     Packet_Sockets.Thin.Payload_Type;
                      Socket           :     Packet_Sockets.Thin.Socket_Type;
                      Response         : out Packet_Sockets.Thin.Payload_Type;
                      Response_Length  : out Natural;
                      From_MAC_Address : out Packet_Sockets.Thin.MAC_Address_Type);

   procedure Validate_DAK_Pass_Phrase (Pass_Phrase      : String;
                                       Check_Min_Length : Boolean := True);

   procedure Validate_HFID (HFID            : HFID_String.Bounded_String;
                            Min_HFID_Length : Positive := 1);

   procedure Validate_NMK_Pass_Phrase (Pass_Phrase      : String;
                                       Check_Min_Length : Boolean := True);

   procedure Validate_Pass_Phrase (Pass_Phrase            : String;
                                   Min_Pass_Phrase_Length : Positive;
                                   Max_Pass_Phrase_Length : Positive;
                                   Check_Min_Length       : Boolean := True);

end Power_Line_Adapter;
