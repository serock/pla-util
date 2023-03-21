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
package OS_Constants is

   AF_PACKET   : constant := 17;
   SOCK_DGRAM  : constant := 2;
   SOL_SOCKET  : constant := 1;
   SO_RCVTIMEO : constant := 20;
   SO_SNDTIMEO : constant := 21;

   ETH_ALEN : constant := 6;

   IFNAMSIZ     : constant := 16;
   SIOCGIFINDEX : constant := 16#8933#;

   EAGAIN      : constant := 11;
   EWOULDBLOCK : constant := 11;

end OS_Constants;
