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
with Ada.Characters.Latin_1;
with Ada.Strings;
with Interfaces.C;

use type Interfaces.Unsigned_8;
use type Interfaces.C.int;
use type Interfaces.C.long;
use type Interfaces.C.unsigned;

package body Ethernet.Datagram_Socket is

   package C_Binding is
      -- bits/socket.h
      C_AF_PACKET  : constant := 17;

      -- asm-generic/errno-base.h
      C_EAGAIN      : constant := 11;

      -- asm-generic/errno.h
      C_EWOULDBLOCK : constant := 11;

      -- linux/if_ether.h
      C_ETH_ALEN : constant := 6;

      -- net/if.h
      C_IF_NAMESIZE : constant := 16;

      -- bits/ioctls.h
      C_SIOCGIFINDEX  : constant := 16#8933#;

      -- bits/socket_type.h
      C_SOCK_DGRAM : constant := 2;

      -- asm-generic/socket.h
      -- gcc -E -dM /usr/include/sys/socket.h | grep TIMEO
      C_SO_RCVTIMEO : constant := 20;
      C_SO_SNDTIMEO : constant := 21;

      -- asm-generic/socket.h
      C_SOL_SOCKET : constant := 1;

      type Int_Pointer is access all Interfaces.C.int;

      -- bits/socket.h
      -- bits/types.h
      subtype socklen_t is Interfaces.C.unsigned;

      -- linux/if_packet.h
      type sockaddr_ll is
         record
            sll_family   : Interfaces.C.unsigned_short;
            sll_protocol : Network_Protocol_Type;
            sll_ifindex  : Interfaces.C.int;
            sll_hatype   : Interfaces.C.unsigned_short;
            sll_pkttype  : Interfaces.C.unsigned_char;
            sll_halen    : Interfaces.C.unsigned_char;
            sll_addr     : Octets_Type;
         end record
        with Convention => C;

      -- linux/time.h
      -- asm-generic/posix_types.h
      type timeval is
         record
            tv_sec  : Interfaces.C.long;
            tv_usec : Interfaces.C.long;
         end record
        with Convention => C;

      -- net/if.h
      type ifreq is
         record
            ifr_name    : Interfaces.C.char_array(1 .. C_IF_NAMESIZE);
            ifr_ifindex : Interfaces.C.int;
         end record
        with Convention => C;

      -- bits/types.h
      -- bits/typesizes.h
      -- gcc -E -dM /usr/include/sys/types.h | grep __SSIZE_T_TYPE
      -- gcc -E -dM /usr/include/sys/types.h | grep __SWORD_TYPE
      subtype ssize_t is Interfaces.C.long; -- valid for 64-bit operating system

      -- unistd.h
      function C_Close(Fd : in Interfaces.C.int) return Interfaces.C.int
        with Import => True, Convention => C, External_Name => "close";

      -- sys/socket.h
      function C_Bind(Fd   : in Interfaces.C.int;
                      Addr : in sockaddr_ll;
                      Len  : in socklen_t) return Interfaces.C.int
        with Import => True, Convention => C, External_Name => "bind";

      -- bits/errno.h
      function C_Errno return Int_Pointer
        with Import => True, Convention => C, External_Name => "__errno_location";

      -- netinet/in.h
      function C_Htons(Hostshort : in Protocol_Type) return Network_Protocol_Type
        with Import => True, Convention => C, External_Name => "htons";

      -- sys/ioctl.h
      function C_Ioctl(Fd      : in     Interfaces.C.int;
                       Request : in     Interfaces.C.unsigned_long;
                       Argp    : in out ifreq) return Interfaces.C.int
        with Import => True, Convention => C, External_Name => "ioctl";

      -- sys/socket.h
      function C_Recvfrom(Fd       : in     Interfaces.C.int;
                          Buf      :    out Payload_Type;
                          N        : in     Interfaces.C.size_t;
                          Flags    : in     Interfaces.C.int    := 0;
                          Addr     :    out sockaddr_ll;
                          Addr_Len : in out socklen_t) return ssize_t
        with Import => True, Convention => C, External_Name => "recvfrom";

      -- sys/socket.h
      function C_Sendto(Fd       : in Interfaces.C.int;
                        Buf      : in Payload_Type;
                        N        : in Interfaces.C.size_t;
                        Flags    : in Interfaces.C.int    := 0;
                        Addr     : in sockaddr_ll;
                        Addr_Len : in socklen_t) return ssize_t
        with Import => True, Convention => C, External_Name => "sendto";

      -- sys/socket.h
      function C_Setsockopt(Fd      : in Interfaces.C.int;
                            Level   : in Interfaces.C.int;
                            Optname : in Interfaces.C.int;
                            Optval  : in timeval;
                            Optlen  : in socklen_t) return Interfaces.C.int
        with Import => True, Convention => C, External_Name => "setsockopt";

      -- sys/socket.h
      function C_Socket(Domain   : in Interfaces.C.int;
                        Kind     : in Interfaces.C.int;
                        Protocol : in Interfaces.C.int) return Interfaces.C.int
        with Import => True, Convention => C, External_Name => "socket";

      -- string.h
      function C_Strerror_r(Errnum : in     Interfaces.C.int;
                            Buf    :    out Interfaces.C.char_array;
                            Buflen : in     Interfaces.C.size_t) return Interfaces.C.int
        with Import => True, Convention => C, External_Name => "__xpg_strerror_r";

   end C_Binding;

   use C_Binding;

   function Errno return Interfaces.C.int is

   begin

      return C_Errno.all;

   end Errno;

   function Error_Message(Error_Number : in Interfaces.C.int) return String is

      Error_Buffer : Interfaces.C.char_array(1 .. 128);
      Return_Value : Interfaces.C.int;

   begin

      Return_Value := C_Strerror_r(Errnum => Error_Number,
                                   Buf    => Error_Buffer,
                                   Buflen => Error_Buffer'Length);

      if Return_Value = 0 then

         return Interfaces.C.To_Ada(Item => Error_Buffer);

      else

         return "";

      end if;

   end Error_Message;

   procedure Bind(File_Descriptor  : in Interfaces.C.int;
                  Network_Protocol : in Network_Protocol_Type;
                  Interface_Index  : in Interfaces.C.int) is

      Socket_Address : sockaddr_ll;
      Return_Value   : Interfaces.C.int;

   begin

      Socket_Address := (sll_family   => C_AF_PACKET,
                         sll_protocol => Network_Protocol,
                         sll_ifindex  => Interface_Index,
                         sll_hatype   => 0,
                         sll_pkttype  => 0,
                         sll_halen    => 0,
                         sll_addr     => (others => 0));

      Return_Value := C_Bind(Fd   => File_Descriptor,
                             Addr => Socket_Address,
                             Len  => Socket_Address'Size / 8);

      if Return_Value = -1 then

         raise Socket_Error with Error_Message(Error_Number => Errno);

      end if;

   end Bind;

   function Get_Interface_Index(Device_Name : in String;
                                Fd          : in Interfaces.C.int) return Interfaces.C.int is
      If_Req        : ifreq;
      Count         : Interfaces.C.size_t;
      Return_Value  : Interfaces.C.int;

   begin

      Interfaces.C.To_C(Item   => Device_Name,
                        Target => If_Req.ifr_name,
                        Count  => Count);

      Return_Value := C_Ioctl(Fd      => Fd,
                              Request => C_SIOCGIFINDEX,
                              Argp    => If_Req);

      if Return_Value = -1 then

         raise Socket_Error with Error_Message(Error_Number => Errno);

      end if;

      return If_Req.ifr_ifindex;

   end Get_Interface_Index;

   procedure Set_Socket_Timeout_Option(File_Descriptor : in Interfaces.C.int;
                                       Option_Name     : in Interfaces.C.int;
                                       Timeout         : in Milliseconds_Type) is

      Option_Value : timeval;
      Return_Value : Interfaces.C.int;

   begin

      Option_Value := (tv_sec  => Interfaces.C.long(Timeout / 1000),
                       tv_usec => Interfaces.C.long((Timeout rem 1000) * 1000));

      Return_Value := C_Setsockopt(Fd      => File_Descriptor,
                                   Level   => C_SOL_SOCKET,
                                   Optname => Option_Name,
                                   Optval  => Option_Value,
                                   Optlen  => timeval'Size / 8);

      if Return_Value = -1 then

         raise Socket_Error with Error_Message(Error_Number => Errno);

      end if;

   end Set_Socket_Timeout_Option;

   procedure Close_Quietly(File_Descriptor : in out Interfaces.C.int) is

      Return_Value : Interfaces.C.int;

   begin

      if File_Descriptor = -1 then

         return;

      end if;

      Return_Value := C_Close(Fd => File_Descriptor);

      File_Descriptor := -1;

   end Close_Quietly;

   function Is_Open(Socket : in Socket_Type) return Boolean is

   begin

      return Socket.File_Descriptor /= -1;

   end Is_Open;

   procedure Close(Socket : in out Socket_Type) is

      Return_Value : Interfaces.C.int;

   begin

      if Socket.Is_Open then

         Return_Value := C_Close(Fd => Socket.File_Descriptor);

         Socket.File_Descriptor := -1;
         Socket.Interface_Index := -1;

         if Return_Value = -1 then

            raise Socket_Error with Error_Message(Error_Number => Errno);

         end if;

      end if;

   end Close;

   function Create_Socket(Network_Protocol : in Network_Protocol_Type) return Interfaces.C.int is

      File_Descriptor : Interfaces.C.int;

   begin

      File_Descriptor := C_Socket(Domain   => C_AF_PACKET,
                                  Kind     => C_SOCK_DGRAM,
                                  Protocol => Interfaces.C.Int(Network_Protocol));

      if File_Descriptor = -1 then

         raise Socket_Error with Error_Message(Error_Number => Errno);

      end if;

      return File_Descriptor;

   end Create_Socket;

   procedure Open(Socket          : in out Socket_Type;
                  Protocol        : in     Protocol_Type;
                  Device_Name     : in     String;
                  Receive_Timeout : in     Milliseconds_Type := 0;
                  Send_Timeout    : in     Milliseconds_Type := 0) is

      File_Descriptor  : Interfaces.C.int;
      Interface_Index  : Interfaces.C.int;
      Network_Protocol : Network_Protocol_Type;

   begin

      if Socket.File_Descriptor /= -1 then

         raise Socket_Error with "Socket is already open";

      end if;

      Network_Protocol := C_Htons(Hostshort => Protocol);

      File_Descriptor := Create_Socket(Network_Protocol => Network_Protocol);

      Interface_Index := Get_Interface_Index(Device_Name => Device_Name,
                                             Fd          => File_Descriptor);

      Bind(File_Descriptor  => File_Descriptor,
           Network_Protocol => Network_Protocol,
           Interface_Index  => Interface_Index);

      Set_Socket_Timeout_Option(File_Descriptor => File_Descriptor,
                                Option_Name     => C_SO_RCVTIMEO,
                                Timeout         => Receive_Timeout);

      Set_Socket_Timeout_Option(File_Descriptor => File_Descriptor,
                                Option_Name     => C_SO_SNDTIMEO,
                                Timeout         => Send_Timeout);

      Socket.File_Descriptor  := File_Descriptor;
      Socket.Network_Protocol := Network_Protocol;
      Socket.Interface_Index  := Interface_Index;

   exception

      when Socket_Error =>

         if File_Descriptor /= -1 then

            Close_Quietly(File_Descriptor => File_Descriptor);

         end if;

         raise;

   end Open;

   procedure Receive(Socket         : in     Socket_Type;
                     Payload        :    out Payload_Type;
                     Payload_Length :    out Natural;
                     From           :    out MAC_Address_Type) is

      Return_Value  : ssize_t;
      Source        : sockaddr_ll;
      Source_Length : socklen_t := Source'Size / 8;

   begin

      Source := (sll_family   => 0,
                 sll_protocol => 0,
                 sll_ifindex  => 0,
                 sll_hatype   => 0,
                 sll_pkttype  => 0,
                 sll_halen    => 0,
                 sll_addr     => (others => 0));

      Return_Value := C_Recvfrom(Fd       => Socket.File_Descriptor,
                                 Buf      => Payload,
                                 N        => Payload'Length,
                                 Addr     => Source,
                                 Addr_Len => Source_Length);

      if Return_Value = -1 then

         if Errno = C_EAGAIN or else Errno = C_EWOULDBLOCK then

            -- Call timed out before any data was received.
            -- There is no message and no source address.

            Payload_Length := 0;
            From           := Null_Address;

         else

            raise Socket_Error with Error_Message(Error_Number => Errno);

         end if;

      else

         Payload_Length := Natural(Return_Value);
         From           := MAC_Address_Type'(Octets => Source.sll_addr);

      end if;

   end Receive;

   procedure Send(Socket  : in Socket_Type;
                  Payload : in Payload_Type;
                  To      : in MAC_Address_Type) is

      Destination  : sockaddr_ll;
      Return_Value : ssize_t;

   begin

      -- Check whether the payload is large enough for the size of the resulting Ethernet packet to
      -- be at least 60 bytes. The packet will have a 14-byte header (6-byte destination address,
      -- 6-byte source address, and 2-byte protocol or ethertype). Thus, the payload size must be
      -- at least 60 bytes - 14 bytes = 46 bytes. This assumes that there is no VLAN info in the
      -- header.

      if Payload'Length < Minimum_Payload_Size then

         raise Socket_Error with "Payload size is less than" & Positive'Image(Minimum_Payload_Size) & " bytes";

      end if;

      Destination := (sll_family   => C_AF_PACKET,
                      sll_protocol => Socket.Network_Protocol,
                      sll_ifindex  => Socket.Interface_Index,
                      sll_hatype   => 0,
                      sll_pkttype  => 0,
                      sll_halen    => C_ETH_ALEN,
                      sll_addr     => To.Octets);

      Return_Value := C_Sendto(Fd       => Socket.File_Descriptor,
                               Buf      => Payload,
                               N        => Payload'Length,
                               Addr     => Destination,
                               Addr_Len => Destination'Size / 8);

      if Return_Value = -1 then

         raise Socket_Error with Error_Message(Error_Number => Errno);

      end if;

   end Send;

   function To_HFID_String(Payload : Payload_Type) return HFID_String.Bounded_String is

      C    : Character;
      P    : Natural := Payload'First(1);
      HFID : HFID_String.Bounded_String;

   begin

      while P <= Payload'Last(1) loop

         C := Character'Val(Payload(P));

         if C = Ada.Characters.Latin_1.NUL then

            exit;

         end if;

         HFID_String.Append(Source   => HFID,
                            New_Item => C,
                            Drop     => Ada.Strings.Error);

         P := P + 1;

      end loop;

      return HFID;

   end To_HFID_String;

end Ethernet.Datagram_Socket;
