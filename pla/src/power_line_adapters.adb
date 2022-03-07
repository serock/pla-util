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
with Ada.Characters.Latin_1;
with GNAT.SHA256;

package body Power_Line_Adapters is

   function "<" (Left  : Adapter_Type;
                 Right : Adapter_Type) return Boolean is

      use type MAC_Addresses.MAC_Address_Type;

   begin

      if Left.Network_Interface = Right.Network_Interface then
         return Left.MAC_Address < Right.MAC_Address;
      else
         return Left.Network_Interface < Right.Network_Interface;
      end if;

   end "<";

   overriding function "=" (Left  : Adapter_Type;
                            Right : Adapter_Type) return Boolean is

      use type MAC_Addresses.MAC_Address_Type;

   begin

      return Left.Network_Interface = Right.Network_Interface and then Left.MAC_Address = Right.MAC_Address;

   end "=";

   procedure Create (Adapter           : in out Adapter_Type;
                     Network_Interface :        Network_Interface_Type;
                     MAC_Address       :        MAC_Addresses.MAC_Address_Type;
                     HFID              :        HFID_Strings.Bounded_String) is

   begin

      Adapter.Network_Interface := Network_Interface;
      Adapter.MAC_Address       := MAC_Address;
      Adapter.HFID              := HFID;

   end Create;

   function Derive_Protocol (Payload : Packets.Payload_Type) return Packets.Protocol_Type is

      use type Octets.Octet_Type;

   begin

      return (if Payload (3) = 16#a0# then Power_Line_Adapters.Protocol_Mediaxtream else Power_Line_Adapters.Protocol_Homeplug);

   end Derive_Protocol;

   function Generate_DAK (Pass_Phrase : String) return Key_Type is

      Salt : constant Ada.Streams.Stream_Element_Array (1 .. 8) := (16#08#, 16#85#, 16#6d#, 16#af#, 16#7c#, 16#f5#, 16#81#, 16#85#);

   begin

      return Generate_Key (Pass_Phrase => Pass_Phrase,
                           Salt        => Salt);

   end Generate_DAK;

   function Generate_Key (Pass_Phrase : String;
                          Salt        : Ada.Streams.Stream_Element_Array) return Key_Type is

      use type Ada.Streams.Stream_Element_Offset;

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
         Key (I) := Octets.Octet_Type (Digest (J));
         J       := J + 1;
      end loop;

      return Key;

   end Generate_Key;

   function Generate_NMK (Pass_Phrase : String) return Key_Type is

      Salt : constant Ada.Streams.Stream_Element_Array (1 .. 8) := (16#08#, 16#85#, 16#6d#, 16#af#, 16#7c#, 16#f5#, 16#81#, 16#86#);

   begin

      return Generate_Key (Pass_Phrase => Pass_Phrase,
                           Salt        => Salt);

   end Generate_NMK;

   function Get_Octets (HFID : HFID_Strings.Bounded_String) return HFID_Octets_Type is

      Length : constant HFID_Strings.Length_Range := HFID_Strings.Length (Source => HFID);
      Octets : HFID_Octets_Type                   := (others => 16#00#);

   begin

      for I in 1 .. Length loop
         Octets (I) := Character'Pos (HFID_Strings.Element (Source => HFID,
                                                            Index  => I));
      end loop;

      return Octets;

   end Get_Octets;

   function Has_MAC_Address (Self        : Adapter_Type;
                             MAC_Address : MAC_Addresses.MAC_Address_Type) return Boolean is

      use type MAC_Addresses.MAC_Address_Type;

   begin

      return Self.MAC_Address = MAC_Address;

   end Has_MAC_Address;

   function Image (Self : Adapter_Type) return String is

   begin

      return Self.MAC_Address.Image & " via " & Self.Network_Interface'Image & " interface, HFID: " & HFID_Strings.To_String (Source => Self.HFID);

   end Image;

   procedure Process (Self             :     Adapter_Type;
                      Request          :     Packet_Sockets.Thin.Payload_Type;
                      Socket           :     Packet_Sockets.Thin.Socket_Type;
                      Response         : out Packet_Sockets.Thin.Payload_Type;
                      Response_Length  : out Natural;
                      From_MAC_Address : out MAC_Addresses.MAC_Address_Type) is

   begin

      Socket.Send (Payload => Request,
                   To      => Self.MAC_Address);

      Socket.Receive (Payload        => Response,
                      Payload_Length => Response_Length,
                      From           => From_MAC_Address);

      if Response_Length = 0 then
         raise Adapter_Error with Message_No_Response;
      end if;

   end Process;

   procedure Validate_DAK_Pass_Phrase (Pass_Phrase      : String;
                                       Check_Min_Length : Boolean := True) is

      Max_Pass_Phrase_Length : constant := 19;
      Min_Pass_Phrase_Length : constant := 19;

   begin

      Validate_Pass_Phrase (Pass_Phrase            => Pass_Phrase,
                            Min_Pass_Phrase_Length => Min_Pass_Phrase_Length,
                            Max_Pass_Phrase_Length => Max_Pass_Phrase_Length,
                            Check_Min_Length       => Check_Min_Length);

   end Validate_DAK_Pass_Phrase;

   procedure Validate_HFID (HFID            : HFID_Strings.Bounded_String;
                            Min_HFID_Length : Positive := 1) is

      Length : constant HFID_Strings.Length_Range := HFID_Strings.Length (HFID);
      C      : Character;

   begin

      if Length < Min_HFID_Length then
         raise Adapter_Error with "HFID has fewer than" & Integer'Image (Min_HFID_Length) & " characters";
      end if;

      for I in 1 .. Length loop

         C := HFID_Strings.Element (Source => HFID,
                                    Index  => I);

         if C < ' ' or else C > Ada.Characters.Latin_1.DEL then
            raise Adapter_Error with "HFID contains one or more illegal characters";
         end if;

      end loop;

   end Validate_HFID;

   procedure Validate_NMK_Pass_Phrase (Pass_Phrase      : String;
                                       Check_Min_Length : Boolean := True) is

      Max_Pass_Phrase_Length : constant := 64;
      Min_Pass_Phrase_Length : constant := 24;

   begin

      Validate_Pass_Phrase (Pass_Phrase            => Pass_Phrase,
                            Min_Pass_Phrase_Length => Min_Pass_Phrase_Length,
                            Max_Pass_Phrase_Length => Max_Pass_Phrase_Length,
                            Check_Min_Length       => Check_Min_Length);

   end Validate_NMK_Pass_Phrase;

   procedure Validate_Pass_Phrase (Pass_Phrase            : String;
                                   Min_Pass_Phrase_Length : Positive;
                                   Max_Pass_Phrase_Length : Positive;
                                   Check_Min_Length       : Boolean := True) is

   begin

      if Pass_Phrase'Length > Max_Pass_Phrase_Length then
         raise Adapter_Error with "Pass phrase has more than" & Integer'Image (Max_Pass_Phrase_Length) & " characters";
      end if;

      if Check_Min_Length and then Pass_Phrase'Length < Min_Pass_Phrase_Length then
         raise Adapter_Error with "Pass phrase has fewer than" & Integer'Image (Min_Pass_Phrase_Length) & " characters";
      end if;

      for I in Pass_Phrase'Range loop

         if Pass_Phrase (I) < ' ' or else Pass_Phrase (I) > Ada.Characters.Latin_1.DEL then
            raise Adapter_Error with "Pass phrase contains one or more illegal characters";
         end if;

      end loop;

   end Validate_Pass_Phrase;

   function Check_DAK (Self                : Adapter_Type;
                       Pass_Phrase         : String;
                       Network_Device_Name : String) return Boolean is separate;

   function Check_NMK (Self                : Adapter_Type;
                       Pass_Phrase         : String;
                       Network_Device_Name : String) return Boolean is separate;

   function Get_Any_Network_Info (Self                : Adapter_Type;
                                  Network_Device_Name : String) return Network_Info_List_Type is separate;

   function Get_Capabilities (Self                : Adapter_Type;
                              Network_Device_Name : String) return Capabilities_Type is separate;

   function Get_Discover_List (Self                : Adapter_Type;
                               Network_Device_Name : String) return Discover_List_Type is separate;

   function Get_HFID (Self                : Adapter_Type;
                      Kind                : HFID_Kind_Type;
                      Network_Device_Name : String) return HFID_Strings.Bounded_String is separate;

   function Get_Id_Info (Self                : Adapter_Type;
                         Network_Device_Name : String) return Id_Info_Type is separate;

   function Get_Manufacturer_HFID (Self                : Adapter_Type;
                                   Network_Device_Name : String) return HFID_Strings.Bounded_String is separate;

   function Get_Member_Network_Info (Self                : Adapter_Type;
                                     Network_Device_Name : String) return Network_Info_List_Type is separate;

   function Get_Network_Info (Self                : Adapter_Type;
                              Scope               : Network_Scope_Type;
                              Network_Device_Name : String) return Network_Info_List_Type is separate;

   function Get_Network_Stats (Self                : Adapter_Type;
                               Network_Device_Name : String) return Network_Stats_List_Type is separate;

   function Get_User_HFID (Self                : Adapter_Type;
                           Network_Device_Name : String) return HFID_Strings.Bounded_String is separate;

   procedure Reset (Self                : Adapter_Type;
                    Network_Device_Name : String) is separate;

   procedure Restart (Self                : Adapter_Type;
                      Network_Device_Name : String) is separate;

   procedure Set_HFID (Self                : Adapter_Type;
                       HFID                : HFID_Strings.Bounded_String;
                       Network_Device_Name : String) is separate;

   procedure Set_NMK (Self                : Adapter_Type;
                      Pass_Phrase         : String;
                      Network_Device_Name : String) is separate;

end Power_Line_Adapters;
