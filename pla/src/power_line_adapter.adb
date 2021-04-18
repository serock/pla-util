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
with Ada.Streams;
with Ada.Characters.Latin_1;
with GNAT.SHA256;
with Packet_Sockets.Thin;

use type Ada.Streams.Stream_Element_Offset;
use type Packet_Sockets.Thin.MAC_Address_Type;

package body Power_Line_Adapter is

   subtype HFID_Bytes_Type is Packet_Sockets.Thin.Bytes_Type (1 .. 64);
   subtype Key_Type        is Packet_Sockets.Thin.Bytes_Type (1 .. 16);

   function "<"(Left  : in Adapter_Type;
                Right : in Adapter_Type) return Boolean is
   begin

      if Left.Network_Interface = Right.Network_Interface then

         return Left.MAC_Address < Right.MAC_Address;

      else

         return Left.Network_Interface < Right.Network_Interface;

      end if;

   end "<";

   overriding function "="(Left  : in Adapter_Type;
                           Right : in Adapter_Type) return Boolean is
   begin
      return Left.Network_Interface = Right.Network_Interface and then Left.MAC_Address = Right.MAC_Address;
   end "=";

   function Generate_Key (Pass_Phrase : in String;
                          Salt        : in Ada.Streams.Stream_Element_Array) return Key_Type is

      Key    : Key_Type;
      J      : Ada.Streams.Stream_Element_Offset;
      Digest : GNAT.SHA256.Binary_Message_Digest;
      PS     : Ada.Streams.Stream_Element_Array (1 .. Pass_Phrase'Length + Salt'Length);

   begin

      J := 1;

      for I in Pass_Phrase'Range loop

         PS (J) := Ada.Streams.Stream_Element (Character'Pos (Pass_Phrase (I)));
         J      := J + 1;

      end loop;

      for I in Salt'Range loop

         PS (J) := Salt (I);
         J      := J + 1;

      end loop;

      Digest := GNAT.SHA256.Digest (A => PS);

      for I in 2 .. 1000 loop

         Digest := GNAT.SHA256.Digest (A => Digest);

      end loop;

      J := 1;

      for I in Key_Type'Range loop

         Key (I) := Interfaces.Unsigned_8 (Digest (J));
         J       := J + 1;

      end loop;

      return Key;

   end Generate_Key;

   function Generate_DAK (Pass_Phrase : in String) return Key_Type is

      Salt : constant Ada.Streams.Stream_Element_Array (1 .. 8) := (16#08#, 16#85#, 16#6d#, 16#af#, 16#7c#, 16#f5#, 16#81#, 16#85#);

   begin

      return Generate_Key (Pass_Phrase => Pass_Phrase,
                           Salt        => Salt);

   end Generate_DAK;

   function Generate_NMK (Pass_Phrase : in String) return Key_Type is

      Salt : constant Ada.Streams.Stream_Element_Array (1 .. 8) := (16#08#, 16#85#, 16#6d#, 16#af#, 16#7c#, 16#f5#, 16#81#, 16#86#);

   begin

      return Generate_Key (Pass_Phrase => Pass_Phrase,
                          Salt        => Salt);

   end Generate_NMK;

   function Get_Bytes (HFID : in HFID_String.Bounded_String) return HFID_Bytes_Type is

      Length : constant HFID_String.Length_Range := HFID_String.Length (Source => HFID);

      Bytes : HFID_Bytes_Type := (others => 16#00#);

   begin

      for I in 1 .. Length loop

         Bytes (I) := Character'Pos (HFID_String.Element (Source => HFID,
                                                          Index  => I));

      end loop;

      return Bytes;

   end Get_Bytes;

   procedure Validate_HFID (HFID            : in HFID_String.Bounded_String;
                            Min_HFID_Length : in Positive := 1) is

      Length : constant HFID_String.Length_Range := HFID_String.Length (HFID);

      C : Character;

   begin

      if Length < Min_HFID_Length then

         raise Input_Error with "HFID has fewer than" & Integer'Image (Min_HFID_Length) & " characters";

      end if;

      for I in 1 .. Length loop

         C := HFID_String.Element (Source => HFID,
                                   Index  => I);

         if C < ' ' or else C > Ada.Characters.Latin_1.DEL then

            raise Input_Error with "HFID contains one or more illegal characters";

         end if;

      end loop;

   end Validate_HFID;

   procedure Validate_Pass_Phrase (Pass_Phrase            : in String;
                                   Min_Pass_Phrase_Length : in Positive;
                                   Max_Pass_Phrase_Length : in Positive;
                                   Check_Min_Length       : in Boolean := True) is

   begin

      if Pass_Phrase'Length > Max_Pass_Phrase_Length then

         raise Input_Error with "Pass phrase has more than" & Integer'Image (Max_Pass_Phrase_Length) & " characters";

      end if;

      if Check_Min_Length and then Pass_Phrase'Length < Min_Pass_Phrase_Length then

         raise Input_Error with "Pass phrase has fewer than" & Integer'Image (Min_Pass_Phrase_Length) & " characters";

      end if;

      for I in Pass_Phrase'Range loop

         if Pass_Phrase (I) < ' ' or else Pass_Phrase (I) > Ada.Characters.Latin_1.DEL then

            raise Input_Error with "Pass phrase contains one or more illegal characters";

         end if;

      end loop;

   end Validate_Pass_Phrase;

   procedure Validate_DAK_Pass_Phrase (Pass_Phrase      : in String;
                                       Check_Min_Length : in Boolean := True) is

      Max_Pass_Phrase_Length : constant := 19;
      Min_Pass_Phrase_Length : constant := 19;

   begin

      Validate_Pass_Phrase (Pass_Phrase            => Pass_Phrase,
                            Min_Pass_Phrase_Length => Min_Pass_Phrase_Length,
                            Max_Pass_Phrase_Length => Max_Pass_Phrase_Length,
                            Check_Min_Length       => Check_Min_Length);

   end Validate_DAK_Pass_Phrase;

   procedure Validate_NMK_Pass_Phrase (Pass_Phrase      : in String;
                                       Check_Min_Length : in Boolean := True) is

      Max_Pass_Phrase_Length : constant := 64;
      Min_Pass_Phrase_Length : constant := 24;

   begin

      Validate_Pass_Phrase (Pass_Phrase            => Pass_Phrase,
                            Min_Pass_Phrase_Length => Min_Pass_Phrase_Length,
                            Max_Pass_Phrase_Length => Max_Pass_Phrase_Length,
                            Check_Min_Length       => Check_Min_Length);

   end Validate_NMK_Pass_Phrase;

   procedure Create (Adapter           : in out Adapter_Type;
                     Network_Interface : in     Positive;
                     MAC_Address       : in     Packet_Sockets.Thin.MAC_Address_Type;
                     HFID              : in     HFID_String.Bounded_String) is

   begin

      Adapter.Network_Interface := Network_Interface;
      Adapter.MAC_Address       := MAC_Address;
      Adapter.HFID              := HFID;

   end Create;

   function Get_MAC_Address (Adapter : Adapter_Type) return Packet_Sockets.Thin.MAC_Address_Type is

   begin

      return Adapter.MAC_Address;

   end Get_MAC_Address;

   function Has_MAC_Address (Adapter     : in Adapter_Type;
                             MAC_Address : in String) return Boolean is
   begin

      return Packet_Sockets.Thin.To_String (MAC_Address => Adapter.MAC_Address) = MAC_Address;

   end Has_MAC_Address;

   procedure Process (Adapter          : in     Adapter_Type;
                      Request          : in     Packet_Sockets.Thin.Payload_Type;
                      Socket           : in     Packet_Sockets.Thin.Socket_Type;
                      Response         :    out Packet_Sockets.Thin.Payload_Type;
                      Response_Length  :    out Natural;
                      From_MAC_Address :    out Packet_Sockets.Thin.MAC_Address_Type) is

   begin

      Socket.Send (Payload => Request,
                   To      => Adapter.Get_MAC_Address);

      Socket.Receive (Payload        => Response,
                      Payload_Length => Response_Length,
                      From           => From_MAC_Address);

      if Response_Length = 0 then

         raise Packet_Sockets.Thin.Socket_Error with Packet_Sockets.Thin.Message_No_Response;

      end if;

   end Process;

   function Check_DAK (Adapter     : in Adapter_Type;
                       Pass_Phrase : in String;
                       Socket      : in Packet_Sockets.Thin.Socket_Type) return Boolean is separate;

   function Check_NMK (Adapter     : in Adapter_Type;
                       Pass_Phrase : in String;
                       Socket      : in Packet_Sockets.Thin.Socket_Type) return Boolean is separate;

   function Get_HFID (Arg     : in Interfaces.Unsigned_8;
                      Adapter : in Adapter_Type;
                      Socket  : in Packet_Sockets.Thin.Socket_Type) return HFID_String.Bounded_String is separate;

   function Get_Manufacturer_HFID (Adapter : in Adapter_Type;
                                   Socket  : in Packet_Sockets.Thin.Socket_Type) return HFID_String.Bounded_String is separate;

   function Get_User_HFID (Adapter : in Adapter_Type;
                           Socket  : in Packet_Sockets.Thin.Socket_Type) return HFID_String.Bounded_String is separate;

   function Get_Network_Info (Arg     : in Interfaces.Unsigned_8;
                              Adapter : in Adapter_Type;
                              Socket  : in Packet_Sockets.Thin.Socket_Type) return Network_Info_List_Type is separate;

   function Get_Any_Network_Info (Adapter : in Adapter_Type;
                                  Socket  : in Packet_Sockets.Thin.Socket_Type) return Network_Info_List_Type is separate;

   function Get_Member_Network_Info (Adapter : in Adapter_Type;
                                     Socket  : in Packet_Sockets.Thin.Socket_Type) return Network_Info_List_Type is separate;

   procedure Reset (Adapter : in Adapter_Type;
                    Socket  : in Packet_Sockets.Thin.Socket_Type) is separate;

   procedure Restart (Adapter : in Adapter_Type;
                      Socket  : in Packet_Sockets.Thin.Socket_Type) is separate;

   procedure Set_HFID (Adapter : in Adapter_Type;
                       HFID    : in HFID_String.Bounded_String;
                       Socket  : in Packet_Sockets.Thin.Socket_Type) is separate;

   procedure Set_NMK (Adapter     : in Adapter_Type;
                      Pass_Phrase : in String;
                      Socket      : in Packet_Sockets.Thin.Socket_Type) is separate;

   function To_String (Adapter : in Adapter_Type) return String is

   begin

      return
        Packet_Sockets.Thin.To_String (MAC_Address => Adapter.MAC_Address) & ' ' &
        HFID_String.To_String (Source => Adapter.HFID);

   end To_String;

end Power_Line_Adapter;
