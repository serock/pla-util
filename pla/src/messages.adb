--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------
--  pla-util - A power line adapter utility
--  Copyright (C) 2016-2022 John Serock
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
with Octets;

package body Messages is

   Protocol_Homeplug    : constant Packets.Protocol_Type := (16#88#, 16#e1#);
   Protocol_Mediaxtream : constant Packets.Protocol_Type := (16#89#, 16#12#);

   procedure Initialize (Self    : out Message_Type;
                         Payload :     Packets.Payload_Type) is
   begin

      Self.Payload := Payload;

   end Initialize;

   function Payload (Self : Message_Type) return Packets.Payload_Type is
   begin

      return Self.Payload;

   end Payload;

   function Protocol (Self : Message_Type) return Packets.Protocol_Type is

      use type Octets.Octet_Type;

   begin

      return (if Self.Payload (3) = 16#a0# then Protocol_Mediaxtream else Protocol_Homeplug);

   end Protocol;

end Messages;
