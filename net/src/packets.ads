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
with Octets;
private with Ada.Strings.Bounded;

package Packets is

   Packet_Error : exception;

   Minimum_Payload_Length : constant := 46;

   subtype Payload_Type  is Octets.Octets_Type;
   subtype Protocol_Type is Octets.Octets_Type (1 .. 2);

   function Library_Version return String;

private

   package Interface_Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 16);

end Packets;
