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
with Ada.Command_Line;
with Ada.Text_IO;
with Commands;
with HFID_String;
with Power_Line_Adapter;
with Power_Line_Adapter_Sets;

package body Console is

   procedure Discover_Adapters(Device_Name : in String) is

      Adapters : Power_Line_Adapter_Sets.Set(Capacity => Power_Line_Adapter.Max_Adapters);

   begin

      Adapters := Commands.Discover_Adapters(Device_Name => Device_Name);

      for Adapter of Adapters loop

         Ada.Text_IO.Put_Line(Item => Adapter.To_String);

      end loop;

   end Discover_Adapters;

   procedure Get_HFID(Device_Name : in String) is

      HFID : HFID_String.Bounded_String;

   begin

      HFID := Commands.Get_HFID(Device_Name => Device_Name,
                                HFID_Level  => Commands.HFID_Level_Type'Value(Ada.Command_Line.Argument(Number => 3)));

      Ada.Text_IO.Put_Line(Item => HFID_String.To_String(Source => HFID));

   end Get_HFID;

   function To_Command_Name(Source : in String) return String is

      Command_Name : String(Source'Range(1)) := Source;

   begin

      for I in Command_Name'Range(1) loop

         if Command_Name(I) = '-' then

            Command_Name(I) := '_';

         end if;

      end loop;

      return Command_Name;

   end To_Command_Name;

   procedure Process_Command_Line is

--        Command_Name : constant String := To_Command_Name(Source => Ada.Command_Line.Argument(Number => 2));
--        Device_Name  : constant String := Ada.Command_Line.Argument(Number => 1);

      Command : Commands.Command_Type;

   begin

      declare

         Device_Name  : constant String := Ada.Command_Line.Argument(Number => 1);

      begin

         Command := Commands.Command_Type'Value(To_Command_Name(Source => Ada.Command_Line.Argument(Number => 2)));

         case Command is

            when Commands.Discover_Adapters =>

               Discover_Adapters(Device_Name => Device_Name);

            when Commands.Get_HFID =>

               Get_HFID(Device_Name => Device_Name);

         end case;

      end;

   exception

      when Constraint_Error =>

         Ada.Text_IO.Put_Line(Item => "Try one of the following commands:");
         Ada.Text_IO.New_Line(Spacing => 1);
         Ada.Text_IO.Put_Line(Item => "pla-util <NIC> discover-adapters");
         Ada.Text_IO.Put_Line(Item => "pla-util <NIC> get-hfid manufacturer");
         Ada.Text_IO.Put_Line(Item => "pla-util <NIC> get-hfid user");
         Ada.Text_IO.New_Line(Spacing => 1);
         Ada.Text_IO.Put_Line(Item => "where <NIC> is the name of an ethernet network device");

   end Process_Command_Line;

end Console;
