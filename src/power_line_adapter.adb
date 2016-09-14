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
with Ada.Streams;
with Ada.Characters.Latin_1;
with Ethernet;
with GNAT.SHA256;
with Interfaces;

use type Ada.Streams.Stream_Element_Offset;
use type Ethernet.MAC_Address_Type;

package body Power_Line_Adapter is

   type Network_Membership_Key_Type is array (Positive range 1 .. 16) of Interfaces.Unsigned_8;

   function "<"(Left  : in Adapter_Type;
                Right : in Adapter_Type) return Boolean is
   begin

      if Left.Adapter_Number = Right.Adapter_Number then

         return Left.MAC_Address < Right.MAC_Address;

      else

         return Left.Adapter_Number < Right.Adapter_Number;

      end if;

   end "<";

   function "="(Left  : in Adapter_Type;
                Right : in Adapter_Type) return Boolean is
   begin
      return Left.Adapter_Number = Right.Adapter_Number and Left.MAC_Address = Right.MAC_Address;
   end "=";

   function Generate_NMK(Pass_Phrase : in String) return Network_Membership_Key_Type is

      Salt : constant Ada.Streams.Stream_Element_Array(1 .. 8) := (16#08#, 16#85#, 16#6d#, 16#af#, 16#7c#, 16#f5#, 16#81#, 16#86#);

      NMK    : Network_Membership_Key_Type;
      J      : Ada.Streams.Stream_Element_Offset;
      Digest : GNAT.SHA256.Binary_Message_Digest;
      PS     : Ada.Streams.Stream_Element_Array(1 .. Pass_Phrase'Length + Salt'Length);

   begin

      J := 1;

      for I in Pass_Phrase'Range loop

         PS(J) := Ada.Streams.Stream_Element(Character'Pos(Pass_Phrase(I)));
         J     := J + 1;

      end loop;

      for I in Salt'Range loop

         PS(J) := Salt(I);
         J     := J + 1;

      end loop;

      Digest := GNAT.SHA256.Digest(A => PS);

      for I in 2 .. 1000 loop

         Digest := GNAT.SHA256.Digest(A => Digest);

      end loop;

      J := 1;

      for I in Network_Membership_Key_Type'Range loop

         NMK(I) := Interfaces.Unsigned_8(Digest(J));
         J      := J + 1;

      end loop;

      return NMK;

   end Generate_NMK;

   procedure Validate_Pass_Phrase(Pass_Phrase      : in String;
                                  Check_Min_Length : in Boolean := True) is

      Max_Pass_Phrase_Length : constant := 64;
      Min_Pass_Phrase_Length : constant := 24;

   begin

      if Pass_Phrase'Length > Max_Pass_Phrase_Length then

         raise Input_Error with "Pass phrase has more than" & Integer'Image(Max_Pass_Phrase_Length) & " characters";

      end if;


      if Check_Min_Length and then Pass_Phrase'Length < Min_Pass_Phrase_Length then

         raise Input_Error with "Pass phrase has fewer than" & Integer'Image(Min_Pass_Phrase_Length) & " characters";

      end if;

      for I in Pass_Phrase'Range loop

         if Pass_Phrase(I) < ' ' or else Pass_Phrase(I) > Ada.Characters.Latin_1.DEL then

            raise Input_Error with "Pass phrase contains one or more illegal characters";

         end if;

      end loop;

   end Validate_Pass_Phrase;

   function Check_NMK(Adapter     : in Adapter_Type;
                      Pass_Phrase : in String;
                      Socket      : in Ethernet.Datagram_Socket.Socket_Type) return Boolean is separate;

   procedure Create(Adapter        : in out Adapter_Type;
                    Adapter_Number : in     Positive;
                    MAC_Address    : in     Ethernet.MAC_Address_Type;
                    HFID           : in     HFID_String.Bounded_String) is

   begin

      Adapter.Adapter_Number := Adapter_Number;
      Adapter.MAC_Address    := MAC_Address;
      Adapter.HFID           := HFID;

   end Create;

   function Get_MAC_Address(Adapter : Adapter_Type) return Ethernet.MAC_Address_Type is

   begin

      return Adapter.MAC_Address;

   end Get_MAC_Address;

   function Get_Manufacturer_HFID(Adapter : in Adapter_Type;
                                  Socket  : in Ethernet.Datagram_Socket.Socket_Type) return HFID_String.Bounded_String is separate;

   function Get_User_HFID(Adapter : in Adapter_Type;
                          Socket  : in Ethernet.Datagram_Socket.Socket_Type) return HFID_String.Bounded_String is separate;

   function To_String(Adapter : in Adapter_Type) return String is

   begin

      return
        Adapter.MAC_Address.To_String & ' ' &
        HFID_String.To_String(Source => Adapter.HFID);

   end To_String;

end Power_Line_Adapter;
