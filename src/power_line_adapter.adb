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
package body Power_Line_Adapter is

   use type Ethernet.MAC_Address_Type;

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
