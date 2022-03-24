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
with Octets;
with System;

private package Packets.Pcap is

   DLT_EN10MB      : constant Interfaces.C.int      := 1;
   ERRBUF_SIZE     : constant                       := 256;
   NETMASK_UNKNOWN : constant Interfaces.C.unsigned := 16#ffffffff#;
   POLLIN          : constant                       := 1;

   type BPF_Instruction_Type is
      record
         Code : aliased Interfaces.C.unsigned_short;
         Jt   : aliased Interfaces.C.unsigned_char;
         Jf   : aliased Interfaces.C.unsigned_char;
         K    : aliased Interfaces.C.unsigned;
      end record
     with
       Convention => C_Pass_By_Copy;

   type BPF_Instruction_Access_Type is access BPF_Instruction_Type;

   type BPF_Program_Type is
      record
         Length       : aliased Interfaces.C.unsigned := 0;
         Instructions : BPF_Instruction_Access_Type   := null;
      end record
     with
       Convention => C_Pass_By_Copy;

   type BPF_Program_Access_Type is access all BPF_Program_Type;

   subtype Error_Buffer_Type is Interfaces.C.char_array (1 .. ERRBUF_SIZE);

   type Direction_Type is (D_In_Out, D_In, D_Out)
     with
       Convention => C;

   type Time_Value_Type is
      record
         Seconds      : aliased Interfaces.C.long;
         Microseconds : aliased Interfaces.C.long;
      end record
     with
       Convention => C_Pass_By_Copy;

   type Packet_Header_Type is
      record
         Timestamp      : aliased Time_Value_Type;
         Capture_Length : aliased Interfaces.C.unsigned;
         Packet_Length  : aliased Interfaces.C.unsigned;
      end record
     with
       Convention => C_Pass_By_Copy;

   type Packet_Header_Access_Type is access Packet_Header_Type;

   type Pcap_Type is private;

   type Pcap_Access_Type is access Pcap_Type;

   type Pcap_Stats_Type is
      record
         Received   : aliased Interfaces.C.unsigned;
         Dropped    : aliased Interfaces.C.unsigned;
         IF_Dropped : aliased Interfaces.C.unsigned;
      end record
     with
       Convention => C_Pass_By_Copy;

   type Poll_File_Descriptor_Type is
      record
         File_Descriptor  : aliased Interfaces.C.int;
         Requested_Events : aliased Interfaces.C.short;
         Returned_Events  : aliased Interfaces.C.short;
      end record
     with
       Convention => C_Pass_By_Copy;

   type Poll_File_Descriptors_Type is array (1 .. 1) of Poll_File_Descriptor_Type;

   function Activate (P : Pcap_Access_Type) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_activate";

   procedure Close (P : Pcap_Access_Type)
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_close";

   function Compile (P                 :     Pcap_Access_Type;
                     Filter_Program    : out BPF_Program_Type;
                     Filter_Expression :     Interfaces.C.char_array;
                     Optimize          :     Interfaces.C.int;
                     Netmask           :     Interfaces.C.unsigned := NETMASK_UNKNOWN) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_compile";

   function Create (Network_Device_Name :     Interfaces.C.char_array;
                    Error_Buffer        : out Error_Buffer_Type) return Pcap_Access_Type
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_create";

   function Datalink (P : Pcap_Access_Type) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_datalink";

   procedure Free_Code (Filter_Program : BPF_Program_Access_Type)
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_freecode";

   function Get_Error_Text (P : Pcap_Access_Type) return Interfaces.C.Strings.chars_ptr
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_geterr";

   function Get_Selectable_File_Descriptor (P : Pcap_Access_Type) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_get_selectable_fd";

   function Library_Version return Interfaces.C.Strings.chars_ptr
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_lib_version";

   function Poll (File_Descriptors           : in out Poll_File_Descriptors_Type;
                  Number_Of_File_Descriptors :        Interfaces.C.unsigned_long := 1;
                  Timeout_Milliseconds       :        Interfaces.C.int) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "poll";

   function Receive_Packet (P             :     Pcap_Access_Type;
                            Packet_Header : out Packet_Header_Access_Type;
                            Packet_Data   : out System.Address) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_next_ex";

   function Send_Packet (P           : Pcap_Access_Type;
                         Buffer      : Octets.Octets_Type;
                         Packet_Size : Interfaces.C.int) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_sendpacket";

   function Set_Datalink (P   : Pcap_Access_Type;
                          DLT : Interfaces.C.int) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_set_datalink";

   function Set_Direction (P         : Pcap_Access_Type;
                           Direction : Direction_Type) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_setdirection";

   function Set_Filter (P              : Pcap_Access_Type;
                        Filter_Program : BPF_Program_Access_Type) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_setfilter";

   function Set_Immediate_Mode (P              : Pcap_Access_Type;
                                Immediate_Mode : Interfaces.C.int) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_set_immediate_mode";

   function Set_Nonblock (P            :     Pcap_Access_Type;
                          Nonblock     :     Interfaces.C.int;
                          Error_Buffer : out Error_Buffer_Type) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_setnonblock";

   function Set_Snapshot_Length (P               : Pcap_Access_Type;
                                 Snapshot_Length : Interfaces.C.int) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_set_snaplen";

   function Stats (P     :     Pcap_Access_Type;
                   Stats : out Pcap_Stats_Type) return Interfaces.C.int
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_stats";

   function Status_To_String (Error : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr
     with
       Import        => True,
       Convention    => C,
       External_Name => "pcap_statustostr";

private

   type Pcap_Type is null record
     with
       Convention => C_Pass_By_Copy;

end Packets.Pcap;
