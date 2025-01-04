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
package body Power_Line_Adapters.Constructors is

   function Create (Network_Interface : Network_Interface_Type;
                    MAC_Address       : MAC_Addresses.MAC_Address_Type;
                    HFID              : HFID_Strings.Bounded_String) return Adapter_Type is

      Adapter : Adapter_Type;

   begin

      Adapter.Initialize (Network_Interface => Network_Interface,
                          MAC_Address       => MAC_Address,
                          HFID              => HFID);

      return Adapter;

   end Create;

end Power_Line_Adapters.Constructors;
