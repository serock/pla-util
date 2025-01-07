--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016-2025 John Serock
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
with Config;
with GNAT.OS_Lib;
with Interfaces.C.Strings;
with Packets.Device_Locators;
with Packets.Filters;
with System;

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

      Error_Buffer   : aliased Pcap.Error_Buffer_Type;
      Handle         : Pcap.Pcap_Access_Type;
      Interface_Name : Interface_Name_Strings.Bounded_String;
      Return_Code    : Interfaces.C.int;
      Target_Count   : Interfaces.C.size_t;

   begin

      declare

         Locator : Device_Locators.Device_Locator_Type;

      begin

         Locator.Find_All_Devices;

         Locator.Find (Device_Name       => Network_Device_Name,
                       Interface_Name    => Interface_Name,
                       Interface_Address => Self.MAC_Address);

      end;

      Interfaces.C.To_C (Item   => Interface_Name_Strings.To_String (Source => Interface_Name),
                         Target => Self.Interface_Name,
                         Count  => Target_Count);

      Handle := Pcap.Create (Network_Device_Name  => Self.Interface_Name,
                             Error_Buffer         => Error_Buffer);

      if Handle = null then
         raise Packet_Error with Interfaces.C.To_Ada (Item => Error_Buffer);
      end if;

      Self.Handle := Handle;

      Return_Code := Pcap.Set_Immediate_Mode (P              => Handle,
                                              Immediate_Mode => 1);

      if Return_Code /= 0 then
         raise Packet_Error with Interfaces.C.Strings.Value (Item => Pcap.Status_To_String (Error => Return_Code));
      end if;

      Return_Code := Pcap.Set_Snapshot_Length (P               => Handle,
                                               Snapshot_Length => 1536);

      if Return_Code /= 0 then
         raise Packet_Error with Interfaces.C.Strings.Value (Item => Pcap.Status_To_String (Error => Return_Code));
      end if;

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

      Return_Code := Pcap.Set_Nonblock (P            => Handle,
                                        Nonblock     => 1,
                                        Error_Buffer => Error_Buffer);

      if Return_Code /= 0 then
         raise Packet_Error with Interfaces.C.To_Ada (Item => Error_Buffer);
      end if;

      declare

         Filter : Filters.Filter_Type;

      begin

         Filter.Compile (Expression => "ether proto 0x8912 or ether proto 0x88e1",
                         Handle     => Handle);

         Filter.Apply_To (Handle => Handle);

      end;

   end Open;

   procedure Receive (Self             :     Network_Device_Type;
                      Payload_Buffer   : out Payload_Type;
                      Payload_Length   : out Natural;
                      From_MAC_Address : out MAC_Addresses.MAC_Address_Type) is

      use type Interfaces.C.int;
      use type Interfaces.C.unsigned_short;

      type Retry_Count_Type is range 0 .. 2;

      Capture_Length        : Natural;
      FD                    : Interfaces.C.int;
      Packet_Data_Address   : System.Address;
      Packet_Header_Access  : Pcap.Packet_Header_Access_Type;
      Poll_File_Descriptors : Pcap.Poll_File_Descriptors_Type;
      Retries_Remaining     : Retry_Count_Type := 2;
      Return_Code           : Interfaces.C.int;
      Returned_Events       : Interfaces.C.unsigned_short;

   begin

      FD := Pcap.Get_Selectable_File_Descriptor (P => Self.Handle);

      if FD = -1 then
         raise Packet_Error with "No selectable file descriptor available";
      end if;

      Poll_File_Descriptors := (1 => (File_Descriptor  => FD,
                                      Requested_Events => Pcap.POLLIN,
                                      Returned_Events  => 0));

      loop

         Return_Code := Pcap.Poll (File_Descriptors     => Poll_File_Descriptors,
                                   Timeout_Milliseconds => Interfaces.C.int (Config.Network_Receive_Timeout));

         if Return_Code > 0 then
            null;
         elsif Return_Code = -1 then
            raise Packet_Error with GNAT.OS_Lib.Errno_Message;
         elsif Return_Code = 0 then

            Payload_Length   := 0;
            From_MAC_Address := MAC_Addresses.Null_MAC_Address;

            return;

         end if;

         Returned_Events := Interfaces.C.unsigned_short (Poll_File_Descriptors (1).Returned_Events);

         if (Returned_Events and Pcap.POLLIN) = Pcap.POLLIN then

            Return_Code := Pcap.Receive_Packet (P             => Self.Handle,
                                                Packet_Header => Packet_Header_Access,
                                                Packet_Data   => Packet_Data_Address);

            exit when Return_Code = 1;

            if Return_Code = -1 then
               raise Packet_Error with Interfaces.C.Strings.Value (Item => Pcap.Get_Error_Text (P => Self.Handle));
            elsif Return_Code = 0 then
               if Retries_Remaining = 0 then
                  raise Packet_Error with "All three attempts to read a packet failed";
               end if;
               Retries_Remaining := Retries_Remaining - 1;
            end if;

         elsif (Returned_Events and Pcap.POLLERR) = Pcap.POLLERR then
            raise Packet_Error with "Error condition while waiting to receive packets";
         elsif (Returned_Events and Pcap.POLLHUP) = Pcap.POLLHUP then
            raise Packet_Error with "Hang-up event while waiting to receive packets";
         elsif (Returned_Events and Pcap.POLLNVAL) = Pcap.POLLNVAL then
            raise Packet_Error with "Invalid poll request";
         else
            raise Packet_Error with "No packets available and no error events";
         end if;

      end loop;

      Capture_Length := Natural (Packet_Header_Access.all.Capture_Length);

      if Payload_Buffer'Length < Capture_Length - 14 then
         raise Packet_Error with "Payload buffer is too small";
      end if;

      declare

         Packet_Data : Octets.Octets_Type (1 .. Capture_Length)
           with
             Address => Packet_Data_Address;

      begin

         Payload_Length                       := Capture_Length - 14;
         Payload_Buffer (1 .. Payload_Length) := Packet_Data (15 .. Capture_Length);
         From_MAC_Address                     := MAC_Addresses.Create_MAC_Address (MAC_Address_Octets => Packet_Data (7 .. 12));

      end;

   end Receive;

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
