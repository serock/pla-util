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
with Interfaces.C.Strings;
with Packets.Device_Locators;
with Packets.Filters;

package body Packets.Network_Devices is

   overriding procedure Finalize (Self : in out Network_Device_Type) is
   begin

      if Self.Handle /= null then

         Pcap.Close (P => Self.Handle);

         Self.Handle := null;

      end if;

   end Finalize;

   function Name (Self : Network_Device_Type) return String is
   begin

      return Interfaces.C.To_Ada (Item => Self.Interface_Name);

   end Name;

   procedure Open (Self                : in out Network_Device_Type;
                   Network_Device_Name :        String) is

      use type Interfaces.C.int;

      Error_Buffer : aliased Pcap.Error_Buffer_Type;
      Handle       : Pcap.Pcap_Access_Type;
      Return_Code  : Interfaces.C.int;
      Target_Count : Interfaces.C.size_t;

   begin

      declare

         Locator : Device_Locators.Device_Locator_Type;

      begin

         Self.MAC_Address := Locator.Find (Device_Name => Network_Device_Name);

      end;

      Interfaces.C.To_C (Item   => Network_Device_Name,
                         Target => Self.Interface_Name,
                         Count  => Target_Count);

      Handle := Pcap.Create (Network_Device_Name  => Self.Interface_Name,
                             Error_Buffer         => Error_Buffer);

      if Handle = null then
         raise Packet_Error with Interfaces.C.To_Ada (Item => Error_Buffer);
      end if;

      Self.Handle := Handle;

      Return_Code := Pcap.Activate (P => Handle);

      if Return_Code /= 0 then
         raise Packet_Error with Interfaces.C.Strings.Value (Item => Pcap.Status_To_String (Error => Return_Code));
      end if;

      Return_Code := Pcap.Datalink (P => Handle);

      if Return_Code < 0 then
         raise Packet_Error with Interfaces.C.Strings.Value (Item => Pcap.Status_To_String (Error => Return_Code));
      end if;

      if Return_Code /= Pcap.DLT_EN10MB then

         Return_Code := Pcap.Set_Datalink (P   => Handle,
                                           DLT => Pcap.DLT_EN10MB);

         if Return_Code /= 0 then
            raise Packet_Error with Interfaces.C.Strings.Value (Item => Pcap.Get_Error_Text (P => Handle));
         end if;

      end if;

      Return_Code := Pcap.Set_Direction (P         => Handle,
                                         Direction => Pcap.D_In);

      if Return_Code /= 0 then
         raise Packet_Error with Interfaces.C.Strings.Value (Item => Pcap.Status_To_String (Error => Return_Code));
      end if;

      declare

         Filter : Filters.Filter_Type;

      begin

         Filter.Apply_To (Expression => "ether proto 0x8192 or ether proto 0x88e1",
                          Handle     => Handle);
      end;

   end Open;

   procedure Send (Self        : Network_Device_Type;
                   Payload     : Payload_Type;
                   Protocol    : Protocol_Type;
                   Destination : MAC_Addresses.MAC_Address_Type) is

      use type Interfaces.C.int;
      use type Octets.Octets_Type;

      Packet      : constant Octets.Octets_Type := Destination.Get_Octets & Self.MAC_Address.Get_Octets & Protocol & Payload;
      Return_Code : Interfaces.C.int;

   begin

      Return_Code := Pcap.Send_Packet (P           => Self.Handle,
                                       Buffer      => Packet,
                                       Packet_Size => Packet'Length);

      if Return_Code /= 0 then
         raise Packet_Error with Interfaces.C.Strings.Value (Item => Pcap.Get_Error_Text (P => Self.Handle));
      end if;

   end Send;

end Packets.Network_Devices;
