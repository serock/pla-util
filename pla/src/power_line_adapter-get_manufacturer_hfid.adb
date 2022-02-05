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
separate (Power_Line_Adapter)

function Get_Manufacturer_HFID (Self                : Adapter_Type;
                                Network_Device_Name : String) return HFID_String.Bounded_String is

begin

   return Self.Get_HFID (Kind                => MANUFACTURER,
                         Network_Device_Name => Network_Device_Name);

end Get_Manufacturer_HFID;
