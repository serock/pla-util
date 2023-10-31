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
separate (Console)

procedure Show_Help is
begin
   Show_Version;
   Ada.Text_IO.Put_Line (Item => "A utility for power line adapters with Broadcom chipsets");
   Ada.Text_IO.New_Line (Spacing => 1);
   Ada.Text_IO.Put_Line (Item => "Usage:");
   Ada.Text_IO.Put_Line (Item => "  " & App_Name & " [options] <command> [arguments]");
   Ada.Text_IO.Put_Line (Item => "  " & App_Name & " -h | --help                      Display help and exit");
   Ada.Text_IO.Put_Line (Item => "  " & App_Name & " -V | --version                   Display version and exit");
   Ada.Text_IO.New_Line (Spacing => 1);
   Ada.Text_IO.Put_Line (Item => "Options:");
   Ada.Text_IO.Put_Line (Item => "  -i, --interface=<name>   Network interface to use (e.g., eth0)");
   Ada.Text_IO.Put_Line (Item => "  -p, --pla=<mac-address>  Power line adapter at unicast MAC address");
   Ada.Text_IO.Put_Line (Item => "  -t, --timeout=<ms>       Network timeout in milliseconds [default:" & Positive'Image (Config.Network_Receive_Timeout) & "]");
   Ada.Text_IO.New_Line (Spacing => 1);
   Ada.Text_IO.Put_Line (Item => "Commands:");
   Ada.Text_IO.Put_Line (Item => "  check-dak <pla-passphrase>         Check device access key");
   Ada.Text_IO.Put_Line (Item => "  check-nmk <passphrase>             Check network membership key");
   Ada.Text_IO.Put_Line (Item => "  discover                           Discover power line adapters on subnet");
   Ada.Text_IO.Put_Line (Item => "  get-capabilities                   Get capabilities");
   Ada.Text_IO.Put_Line (Item => "  get-discover-list                  Get discovered PLAs and networks");
   Ada.Text_IO.Put_Line (Item => "  get-hfid ( manufacturer | user )   Get human-friendly id [default: user]");
   Ada.Text_IO.Put_Line (Item => "  get-id-info                        Get identification info");
   Ada.Text_IO.Put_Line (Item => "  get-network-info ( any | member )  Get network information [default: member]");
   Ada.Text_IO.Put_Line (Item => "  get-network-stats                  Get average PHY data rates");
   Ada.Text_IO.Put_Line (Item => "  get-station-info                   Get power line adapter information");
   Ada.Text_IO.Put_Line (Item => "  reset                              Factory reset power line adapter");
   Ada.Text_IO.Put_Line (Item => "  restart                            Restart / reboot power line adapter");
   Ada.Text_IO.Put_Line (Item => "  set-hfid <id>                      Set user human-friendly id");
   Ada.Text_IO.Put_Line (Item => "  set-nmk <passphrase>               Set network membership key");
end Show_Help;
