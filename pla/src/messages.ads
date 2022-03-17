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
with Packets;

package Messages is

   type Message_Type (Payload_Length : Positive) is tagged private;

   procedure Initialize (Self    : out Message_Type;
                         Payload :     Packets.Payload_Type);

   function Payload (Self : Message_Type) return Packets.Payload_Type;

   function Protocol (Self : Message_Type) return Packets.Protocol_Type;

private

   type Message_Type (Payload_Length : Positive) is tagged
      record
         Payload : Packets.Payload_Type (1 .. Payload_Length);
      end record;

end Messages;
