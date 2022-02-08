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
with HFID_String;
with MAC_Addresses;
private with Ada.Streams;
private with Octets;
private with Packet_Sockets.Thin;

use MAC_Addresses;

package Power_Line_Adapter is

   type NID_Type is mod 16#40_0000_0000_0000#;

   type Adapter_Type is tagged private;

   type Data_Rate_Type    is mod 2048;
   type Network_Kind_Type is (IN_HOME_NETWORK, ACCESS_NETWORK);
   type Networks_Type     is mod 256;
   type SNID_Type         is mod 16;
   type Station_Role_Type is (UNASSOC_STA, UNASSOC_CCO, STA, CCO, BACKUP_CCO);
   type Status_Type       is (JOINED, NOT_JOINED_HAVE_NMK, NOT_JOINED_NO_NMK);
   type TEI_Type          is mod 256;

   type Network_Info_Type is
      record
         NID                : NID_Type;
         SNID               : SNID_Type;
         TEI                : TEI_Type;
         CCo_MAC_Address    : MAC_Address_Type;
         BCCo_MAC_Address   : MAC_Address_Type;
         Num_Coord_Networks : Networks_Type;
         Station_Role       : Station_Role_Type;
         Network_Kind       : Network_Kind_Type;
         Status             : Status_Type;
      end record;

   type Network_Info_List_Type is array (Positive range <>) of Network_Info_Type;

   type Network_Stats_Type is
      record
         Destination_Address    : MAC_Address_Type;
         Average_Rate_To_Dest   : Data_Rate_Type;
         Average_Rate_From_Dest : Data_Rate_Type;
      end record;

   type Network_Stats_List_Type is array (Positive range <>) of Network_Stats_Type;

   Max_Adapters : constant := 16;

   Adapter_Error : exception;

   function "<" (Left  : Adapter_Type;
                 Right : Adapter_Type) return Boolean;

   overriding function "=" (Left  : Adapter_Type;
                            Right : Adapter_Type) return Boolean;

   function Check_DAK (Self                : Adapter_Type;
                       Pass_Phrase         : String;
                       Network_Device_Name : String) return Boolean;

   function Check_NMK (Self                : Adapter_Type;
                       Pass_Phrase         : String;
                       Network_Device_Name : String) return Boolean;

   function Get_Any_Network_Info (Self                : Adapter_Type;
                                  Network_Device_Name : String) return Network_Info_List_Type;

   function Get_Manufacturer_HFID (Self                : Adapter_Type;
                                   Network_Device_Name : String) return HFID_String.Bounded_String;

   function Get_Member_Network_Info (Self                : Adapter_Type;
                                     Network_Device_Name : String) return Network_Info_List_Type;

   function Get_Network_Stats (Self                : Adapter_Type;
                               Network_Device_Name : String) return Network_Stats_List_Type;

   function Get_User_HFID (Self                : Adapter_Type;
                           Network_Device_Name : String) return HFID_String.Bounded_String;

   function Has_MAC_Address (Self        : Adapter_Type;
                             MAC_Address : String) return Boolean;

   function Image (Self : Adapter_Type) return String;

   procedure Reset (Self                : Adapter_Type;
                    Network_Device_Name : String);

   procedure Restart (Self                : Adapter_Type;
                      Network_Device_Name : String);

   procedure Set_HFID (Self                : Adapter_Type;
                       HFID                : HFID_String.Bounded_String;
                       Network_Device_Name : String);

   procedure Set_NMK (Self                : Adapter_Type;
                      Pass_Phrase         : String;
                      Network_Device_Name : String);

private

   use Octets;

   Default_Receive_Timeout     : constant        := 250;
   Default_Send_Timeout        : constant        := 250;
   Message_No_Response         : constant String := "No response received from adapter";
   Message_Unexpected_Response : constant String := "Unexpected response received from adapter";

   type HFID_Kind_Type         is (MANUFACTURER, USER);
   type Network_Interface_Type is (MII0, MII1, PLC, SDR);
   type Network_Scope_Type     is (MEMBER, ANY);

   type Adapter_Type is tagged
      record
         Network_Interface : Network_Interface_Type;
         MAC_Address       : MAC_Address_Type;
         HFID              : HFID_String.Bounded_String;
      end record;

   subtype HFID_Octets_Type is Octets_Type (1 .. 64);
   subtype Key_Type         is Octets_Type (1 .. 16);

   procedure Create (Adapter           : in out Adapter_Type;
                     Network_Interface :        Network_Interface_Type;
                     MAC_Address       :        MAC_Address_Type;
                     HFID              :        HFID_String.Bounded_String);

   function Generate_DAK (Pass_Phrase : String) return Key_Type;

   function Generate_Key (Pass_Phrase : String;
                          Salt        : Ada.Streams.Stream_Element_Array) return Key_Type;

   function Generate_NMK (Pass_Phrase : String) return Key_Type;

   function Get_HFID (Self                : Adapter_Type;
                      Kind                : HFID_Kind_Type;
                      Network_Device_Name : String) return HFID_String.Bounded_String;

   function Get_Network_Info (Self                : Adapter_Type;
                              Scope               : Network_Scope_Type;
                              Network_Device_Name : String) return Network_Info_List_Type;

   function Get_Octets (HFID : HFID_String.Bounded_String) return HFID_Octets_Type;

   procedure Process (Self             :     Adapter_Type;
                      Request          :     Packet_Sockets.Thin.Payload_Type;
                      Socket           :     Packet_Sockets.Thin.Socket_Type;
                      Response         : out Packet_Sockets.Thin.Payload_Type;
                      Response_Length  : out Natural;
                      From_MAC_Address : out MAC_Address_Type);

   procedure Validate_DAK_Pass_Phrase (Pass_Phrase      : String;
                                       Check_Min_Length : Boolean := True);

   procedure Validate_HFID (HFID            : HFID_String.Bounded_String;
                            Min_HFID_Length : Positive := 1);

   procedure Validate_NMK_Pass_Phrase (Pass_Phrase      : String;
                                       Check_Min_Length : Boolean := True);

   procedure Validate_Pass_Phrase (Pass_Phrase            : String;
                                   Min_Pass_Phrase_Length : Positive;
                                   Max_Pass_Phrase_Length : Positive;
                                   Check_Min_Length       : Boolean := True);

end Power_Line_Adapter;
