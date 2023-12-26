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
with Octets;
private with Ada.Strings.Bounded;

package Packets is

   Packet_Error : exception;

   Minimum_Payload_Length : constant := 46;

   subtype Payload_Type  is Octets.Octets_Type;
   subtype Protocol_Type is Octets.Octets_Type (1 .. 2);

private

   package Interface_Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 16);

end Packets;
