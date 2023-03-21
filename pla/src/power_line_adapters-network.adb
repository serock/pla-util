--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016-2023 John Serock
--
--  This file is part of pla-util.
--
--  pla-util is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  pla-util is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program. If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------
with Ada.Exceptions;
with Messages.Constructors;
with Packets.Network_Devices;
with Power_Line_Adapters.Constructors;

package body Power_Line_Adapters.Network is

   Network_Device : Packets.Network_Devices.Network_Device_Type;

   function Discover (Network_Device_Name : String;
                      MAC_Address         : MAC_Addresses.MAC_Address_Type := MAC_Addresses.Broadcast_MAC_Address) return Power_Line_Adapter_Sets.Set is

      Adapters  : Power_Line_Adapter_Sets.Set (Capacity => (if MAC_Address.Is_Unicast then 1 else Max_Adapters));
      Receiving : Boolean := True;

   begin

      Network_Device.Open (Network_Device_Name => Network_Device_Name);

      declare

         Request : constant Messages.Message_Type := Messages.Constructors.Create_Discover_Request;

      begin

         Send (Message     => Request,
               Destination => MAC_Address);

      end;

      if MAC_Address.Is_Unicast then

         Receiving := Receive_Discovered_Adapter (Adapters => Adapters);

      else

         while Receiving loop

            Receiving := Receive_Discovered_Adapter (Adapters => Adapters);

         end loop;

      end if;

      return Adapters;

   exception

      when Error : Packets.Packet_Error =>
         raise Adapter_Error with Ada.Exceptions.Exception_Message (Error);

   end Discover;

   procedure Receive (Confirmation        : out Packets.Payload_Type;
                      Confirmation_Length : out Natural;
                      From_MAC_Address    : out MAC_Addresses.MAC_Address_Type) is
   begin

      Network_Device.Receive (Payload_Buffer   => Confirmation,
                              Payload_Length   => Confirmation_Length,
                              From_MAC_Address => From_MAC_Address);

   end Receive;

   function Receive_Discovered_Adapter (Adapters : in out Power_Line_Adapter_Sets.Set) return Boolean is

      use type Octets.Octets_Type;

      Confirmation          : Packets.Payload_Type (1 .. 75);
      Confirmation_Length   : Natural;
      Expected_Confirmation : constant Packets.Payload_Type := (16#02#, 16#71#, 16#a0#, 16#00#, 16#00#, 16#00#, 16#1f#, 16#84#, 16#01#);
      Network_Interface     : Network_Interface_Type;
      PLA_MAC_Address       : MAC_Addresses.MAC_Address_Type;

   begin

      Network_Device.Receive (Payload_Buffer   => Confirmation,
                              Payload_Length   => Confirmation_Length,
                              From_MAC_Address => PLA_MAC_Address);

      if Confirmation_Length = 0 then
         return False;
      end if;

      if Confirmation (Expected_Confirmation'Range) /= Expected_Confirmation then
         raise Adapter_Error with Message_Unexpected_Confirmation;
      end if;

      case Confirmation (10) is
         when 0      => Network_Interface := MII0;
         when 1      => Network_Interface := MII1;
         when 2 | 3  => Network_Interface := PLC;
         when 4      => Network_Interface := SDR;
         when others =>
            raise Adapter_Error with Message_Unexpected_Confirmation;
      end case;

      Adapters.Include (New_Item => Constructors.Create (Network_Interface => Network_Interface,
                                                            MAC_Address       => PLA_MAC_Address,
                                                            HFID              => To_HFID_String (HFID_Octets => Confirmation (12 .. Confirmation_Length))));

      return True;

   end Receive_Discovered_Adapter;

   procedure Send (Message     : Messages.Message_Type;
                   Destination : MAC_Addresses.MAC_Address_Type) is
   begin

      Network_Device.Send (Payload     => Message.Payload,
                           Protocol    => Message.Protocol,
                           Destination => Destination);

   end Send;

end Power_Line_Adapters.Network;
