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
with Ada.Exceptions;
with Ada.Strings;
with Ada.Text_IO;
with Byte_IO;
with Commands;
with HFID_String;
with Power_Line_Adapter;
with Power_Line_Adapter_Sets;

package body Console is

   Message_Too_Few_Arguments : constant String := "Too few arguments";

   Syntax_Error : exception;

   procedure Check_DAK(Device_Name : in String) is

      Is_Match : Boolean;

   begin

      if Ada.Command_Line.Argument_Count < 3 then

         raise Syntax_Error with Message_Too_Few_Arguments;

      end if;

      Is_Match := Commands.Check_DAK(Device_Name => Device_Name,
                                     Pass_Phrase => Ada.Command_Line.Argument(Number => 3));

      if Is_Match then

         Ada.Text_IO.Put_Line(Item => "Device Access Key matches");

      else

         Ada.Text_IO.Put_Line(Item => "Device Access Key does not match");

      end if;

   end Check_DAK;

   procedure Check_NMK(Device_Name : in String) is

      Is_Match : Boolean;

   begin

      if Ada.Command_Line.Argument_Count < 3 then

         raise Syntax_Error with Message_Too_Few_Arguments;

      end if;

      Is_Match := Commands.Check_NMK(Device_Name => Device_Name,
                                     Pass_Phrase => Ada.Command_Line.Argument(Number => 3));

      if Is_Match then

         Ada.Text_IO.Put_Line(Item => "Network Membership Key matches");

      else

         Ada.Text_IO.Put_Line(Item => "Network Membership Key does not match");

      end if;

   end Check_NMK;

   procedure Discover_Adapters(Device_Name : in String) is

      Adapters : Power_Line_Adapter_Sets.Set(Capacity => Power_Line_Adapter.Max_Adapters);

   begin

      Adapters := Commands.Discover_Adapters(Device_Name => Device_Name);

      for Adapter of Adapters loop

         Ada.Text_IO.Put_Line(Item => Adapter.To_String);

      end loop;

   end Discover_Adapters;

   procedure Get_HFID(Device_Name : in String) is

      HFID       : HFID_String.Bounded_String;
      HFID_Level : Commands.HFID_Level_Type;

   begin

      if Ada.Command_Line.Argument_Count < 3 then

         raise Syntax_Error with Message_Too_Few_Arguments;

      end if;

      declare

         HFID_Level_Arg : String := Ada.Command_Line.Argument(Number => 3);

      begin

         HFID_Level := Commands.HFID_Level_Type'Value(HFID_Level_Arg);

      exception

         when Constraint_Error =>

            raise Syntax_Error with "Invalid HFID level """ & HFID_Level_Arg & '"';

      end;

      HFID := Commands.Get_HFID(Device_Name => Device_Name,
                                HFID_Level  => HFID_Level);

      Ada.Text_IO.Put_Line(Item => HFID_String.To_String(Source => HFID));

   end Get_HFID;

   procedure Get_Network_Info(Device_Name : in String) is

      Network_Info_List : Power_Line_Adapter.Network_Info_List_Type := Commands.Get_Network_Info(Device_Name => Device_Name);

   begin

      if Network_Info_List'Length = 0 then

         Ada.Text_IO.Put_Line(Item => "Adapter is not a member of any network");

      else

         Ada.Text_IO.Put(Item => "NID:  ");

         for I in Network_Info_List(1).NID'Range loop

            Byte_IO.Put(Item  => Network_Info_List(1).NID(I));

         end loop;

         Ada.Text_IO.New_Line(Spacing => 1);

         Ada.Text_IO.Put(Item => "SNID: ");

         Byte_IO.Put(Item  => Network_Info_List(1).SNID);

         Ada.Text_IO.New_Line(Spacing => 1);

         Ada.Text_IO.Put(Item => "TEI:  ");

         Byte_IO.Put(Item  => Network_Info_List(1).TEI);

         Ada.Text_IO.New_Line(Spacing => 1);

      end if;

   end Get_Network_Info;

   function To_Command_Name(Source : in String) return String is

      Command_Name : String(Source'Range) := Source;

   begin

      for I in Command_Name'Range loop

         if Command_Name(I) = '-' then

            Command_Name(I) := '_';

         end if;

      end loop;

      return Command_Name;

   end To_Command_Name;

   procedure Process_Command_Line is

      Command : Commands.Command_Type;

   begin

      if Ada.Command_Line.Argument_Count < 2 then

         raise Syntax_Error with Message_Too_Few_Arguments;

      end if;

      declare

         Device_Name : constant String := Ada.Command_Line.Argument(Number => 1);

      begin

         Command := Commands.Command_Type'Value(To_Command_Name(Source => Ada.Command_Line.Argument(Number => 2)));

         case Command is

            when Commands.Check_DAK =>

               Check_DAK(Device_Name => Device_Name);

            when Commands.Check_NMK =>

               Check_NMK(Device_Name => Device_Name);

            when Commands.Discover_Adapters =>

               Discover_Adapters(Device_Name => Device_Name);

            when Commands.Get_HFID =>

               Get_HFID(Device_Name => Device_Name);

            when Commands.Get_Network_Info =>

               Get_Network_Info(Device_Name => Device_Name);

         end case;

      end;

   exception

      when Error: Syntax_Error =>

         Ada.Text_IO.Put_Line(Item => "Error: " & Ada.Exceptions.Exception_Message(X => Error));
         Ada.Text_IO.New_Line(Spacing => 1);
         Ada.Text_IO.Put_Line(Item => "Try one of the following commands:");
         Ada.Text_IO.New_Line(Spacing => 1);
         Ada.Text_IO.Put_Line(Item => "pla-util <NIC> discover-adapters");
         Ada.Text_IO.Put_Line(Item => "pla-util <NIC> get-hfid manufacturer");
         Ada.Text_IO.Put_Line(Item => "pla-util <NIC> get-hfid user");
         Ada.Text_IO.Put_Line(Item => "pla-util <NIC> check-dak <plc-pass-phrase>");
         Ada.Text_IO.Put_Line(Item => "pla-util <NIC> check-nmk <pass-phrase>");
         Ada.Text_IO.Put_Line(Item => "pla-util <NIC> get-network-info");
         Ada.Text_IO.New_Line(Spacing => 1);
         Ada.Text_IO.Put_Line(Item => "where <NIC> is the name of an ethernet network device");

      when Error: Commands.Command_Error | Power_Line_Adapter.Input_Error =>

         Ada.Text_IO.Put_Line(Item => "Error: " & Ada.Exceptions.Exception_Message(X => Error));

   end Process_Command_Line;

end Console;
