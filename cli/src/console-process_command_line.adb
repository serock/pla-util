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
with Ada.Exceptions;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with GNAT.Command_Line;

separate (Console)

procedure Process_Command_Line is

   package String_Hashed_Maps is new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                                            Element_Type    => String,
                                                                            Hash            => Ada.Strings.Hash,
                                                                            Equivalent_Keys => "=");

   procedure Process_Help_Switch;
   procedure Process_Interface_Switch;
   procedure Process_PLA_Switch;
   procedure Process_Timeout_Switch;
   procedure Process_Version_Switch;

   I_Key                       : constant String := "interface";
   Message_Invalid_PLA_Address : constant String := "Invalid PLA address";
   Message_Missing_Command     : constant String := "Missing command";
   Message_Missing_HFID        : constant String := "Missing HFID argument";
   Message_Missing_Passphrase  : constant String := "Missing passphrase argument";
   P_Key                       : constant String := "pla";
   Switch_Parameter_Map        : String_Hashed_Maps.Map;
   Timeout                     : Positive        := Config.Network_Receive_Timeout;
   Version_Option              : Boolean         := False;

   procedure Process_Help_Switch is
   begin
      raise GNAT.Command_Line.Exit_From_Command_Line;
   end Process_Help_Switch;

   procedure Process_Interface_Switch is
   begin
      Switch_Parameter_Map.Include (Key      => I_Key,
                                    New_Item => GNAT.Command_Line.Parameter);
   end Process_Interface_Switch;

   procedure Process_PLA_Switch is
   begin
      Switch_Parameter_Map.Include (Key      => P_Key,
                                    New_Item => GNAT.Command_Line.Parameter);

   end Process_PLA_Switch;

   procedure Process_Timeout_Switch is
   begin
      Timeout := Integer'Value (GNAT.Command_Line.Parameter);
   exception
      when others =>
         raise Syntax_Error with "Invalid timeout '" & GNAT.Command_Line.Parameter & "'";
   end Process_Timeout_Switch;

   procedure Process_Version_Switch is
   begin
      Version_Option := True;
   end Process_Version_Switch;

begin

   Switch_Parameter_Map.Include (Key      => I_Key,
                                 New_Item => "");

   Switch_Parameter_Map.Include (Key      => P_Key,
                                 New_Item => "");

   GNAT.Command_Line.Initialize_Option_Scan (Stop_At_First_Non_Switch => True);

   loop

      case GNAT.Command_Line.Getopt (Switches => "h -help V -version i: -interface= p: -pla= t: -timeout=") is

         when ASCII.NUL => exit;

            when 'h' =>

            if GNAT.Command_Line.Full_Switch = "h" then
               Process_Help_Switch;
            end if;

         when 'V' =>

            if GNAT.Command_Line.Full_Switch = "V" then
               Process_Version_Switch;
            end if;

         when 'i' =>

            if GNAT.Command_Line.Full_Switch = "i" then
               Process_Interface_Switch;
            end if;

         when 'p' =>

            if GNAT.Command_Line.Full_Switch = "p" then
               Process_PLA_Switch;
            end if;

         when 't' =>

            if GNAT.Command_Line.Full_Switch = "t" then

               Process_Timeout_Switch;

            end if;

         when '-' =>

            if GNAT.Command_Line.Full_Switch = "-help" then

               Process_Help_Switch;

            elsif GNAT.Command_Line.Full_Switch = "-version" then

               Process_Version_Switch;

            elsif GNAT.Command_Line.Full_Switch = "-interface" then

               Process_Interface_Switch;

            elsif GNAT.Command_Line.Full_Switch = "-pla" then

               Process_PLA_Switch;

            elsif GNAT.Command_Line.Full_Switch = "-timeout" then

               Process_Timeout_Switch;

            end if;

         when others => raise Program_Error;

      end case;

   end loop;

   if Version_Option then
      Show_Version;
      return;
   end if;

   if Timeout /= Config.Network_Receive_Timeout then
      Config.Network_Receive_Timeout := Timeout;
   end if;

   declare

      Command             : Commands.Command_Type;
      Command_Id          : constant String                         := GNAT.Command_Line.Get_Argument;
      Network_Device_Name : constant String                         := Switch_Parameter_Map (I_Key);
      PLA_MAC_Address     : constant MAC_Addresses.MAC_Address_Type := (if Switch_Parameter_Map (P_Key) = "" then MAC_Addresses.Broadcast_MAC_Address else
                                                                           Get_PLA_MAC_Address (Image => Switch_Parameter_Map (P_Key)));

   begin

      if Switch_Parameter_Map (P_Key) /= "" and then not PLA_MAC_Address.Is_Unicast then
         raise Syntax_Error with Message_Invalid_PLA_Address;
      end if;

      if Command_Id = "" then
         raise Syntax_Error with Message_Missing_Command;
      end if;

      Command := To_Command (Source => Command_Id);

      case Command is
         when Commands.Check_DAK =>

            declare

               Passphrase : constant String := GNAT.Command_Line.Get_Argument;

            begin

               if Passphrase = "" then
                  raise Syntax_Error with Message_Missing_Passphrase;
               end if;

               Check_DAK (Network_Device_Name => Network_Device_Name,
                          Passphrase          => Passphrase,
                          PLA_MAC_Address     => PLA_MAC_Address);

            end;

         when Commands.Check_NMK =>

            declare

               Passphrase : constant String := GNAT.Command_Line.Get_Argument;

            begin

               if Passphrase = "" then
                  raise Syntax_Error with Message_Missing_Passphrase;
               end if;

               Check_NMK (Network_Device_Name => Network_Device_Name,
                          Passphrase          => Passphrase,
                          PLA_MAC_Address     => PLA_MAC_Address);

            end;

         when Commands.Discover =>

            Discover (Network_Device_Name => Network_Device_Name,
                      PLA_MAC_Address     => PLA_MAC_Address);

         when Commands.Get_Capabilities =>

            Get_Capabilities (Network_Device_Name => Network_Device_Name,
                              PLA_MAC_Address     => PLA_MAC_Address);

         when Commands.Get_Discover_List =>

            Get_Discover_List (Network_Device_Name => Network_Device_Name,
                               PLA_MAC_Address     => PLA_MAC_Address);

         when Commands.Get_HFID =>

            declare

               HFID_Level_Text : constant String := GNAT.Command_Line.Get_Argument;

            begin

               Get_HFID (Network_Device_Name => Network_Device_Name,
                         HFID_Level          => (if HFID_Level_Text = "" then Commands.User else Get_HFID_Level (HFID_Level_Text => HFID_Level_Text)),
                         PLA_MAC_Address     => PLA_MAC_Address);

            end;

         when Commands.Get_Id_Info =>

            Get_Id_Info (Network_Device_Name => Network_Device_Name,
                         PLA_MAC_Address     => PLA_MAC_Address);

         when Commands.Get_Network_Info =>

            declare

               Network_Scope_Text : constant String := GNAT.Command_Line.Get_Argument;

            begin

               Get_Network_Info (Network_Device_Name => Network_Device_Name,
                                 Network_Scope       => (if Network_Scope_Text = "" then Commands.Member else Get_Network_Scope (Network_Scope_Text => Network_Scope_Text)),
                                 PLA_MAC_Address     => PLA_MAC_Address);

            end;

         when Commands.Get_Network_Stats =>

            Get_Network_Stats (Network_Device_Name => Network_Device_Name,
                               PLA_MAC_Address     => PLA_MAC_Address);

         when Commands.Reset =>

            Reset (Network_Device_Name => Network_Device_Name,
                   PLA_MAC_Address     => PLA_MAC_Address);

         when Commands.Restart =>

            Restart (Network_Device_Name => Network_Device_Name,
                     PLA_MAC_Address     => PLA_MAC_Address);

         when Commands.Set_HFID =>

            declare

               HFID : constant String := GNAT.Command_Line.Get_Argument;

            begin

               if HFID = "" then
                  raise Syntax_Error with Message_Missing_HFID;
               end if;

               Set_HFID (Network_Device_Name => Network_Device_Name,
                         HFID                => HFID,
                         PLA_MAC_Address     => PLA_MAC_Address);

            end;

         when Commands.Set_NMK =>

            declare

               Passphrase : constant String := GNAT.Command_Line.Get_Argument;

            begin

               if Passphrase = "" then
                  raise Syntax_Error with Message_Missing_Passphrase;
               end if;

               Set_NMK (Network_Device_Name => Network_Device_Name,
                        Passphrase          => Passphrase,
                        PLA_MAC_Address     => PLA_MAC_Address);

            end;
      end case;

   end;

exception

   when GNAT.Command_Line.Exit_From_Command_Line =>

      Show_Help;

   when Error : Syntax_Error | GNAT.Command_Line.Invalid_Switch =>

      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => App_Name & ": " & Ada.Exceptions.Exception_Message (X => Error));
      GNAT.Command_Line.Try_Help;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);

   when GNAT.Command_Line.Invalid_Parameter =>

      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => App_Name & ": Missing parameter for '-" & GNAT.Command_Line.Full_Switch & "'");
      GNAT.Command_Line.Try_Help;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);

   when Error : Commands.Command_Error | Power_Line_Adapters.Adapter_Error | MAC_Addresses.MAC_Address_Error =>

      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => App_Name & ": " & Ada.Exceptions.Exception_Message (X => Error));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);

   when others =>

      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);

      raise;

end Process_Command_Line;
