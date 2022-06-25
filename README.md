# pla-util
A power line adapter utility

## Overview
The purpose of this program is to allow Linux users to manage HomePlug AV2 compliant power line adapters that use a Broadcom system-on-a-chip (SoC) such as the BCM60500 or BCM60333.

### Compatible Power Line Adapters
The following power line adapters have been used with `pla-util`:

* D-Link DHP-700AV (uses BCM60500)
* NETGEAR PLP1000 (uses BCM60333)
* NETGEAR PLP2000 (uses BCM60500)
* Tenda PH3 (uses BCM60355)
* TP-Link TL-PA7017 (uses BCM60355)
* TP-Link TL-PA9020 (uses BCM60500)

## Development Environment
The following software is being used to develop `pla-util`:

* [GNAT Community Edition](https://www.adacore.com/download), which includes the GNAT Studio IDE and the GPRbuild tool
* [Git](https://git-scm.com/)
* [libpcap](https://www.tcpdump.org/)
* [openSUSE Leap](https://www.opensuse.org/)

### Software
The following library is required to build and run `pla-util`:

* libpcap (version 1.8.1 or higher is recommended; version 1.9.1 was used during development)

### Hardware
During development, `pla-util` is tested with D-Link DHP-700AV adapters from a *D-Link DHP-701AV PowerLine AV2 2000 Gigabit Starter Kit*.

## How to Build the Program with GNAT Studio
1. Use git to clone this repository.
2. Launch `gnatstudio`, select **Open existing project**, and browse to the `pla_util.gpr` project file.
3. Use GNAT Studio's **Build All** toolbar button to build the project, or try **Build** > **Project** > **Build All**.

If the build is successful, the `pla-util` executable will be in the `bin` subdirectory.

## How to Build the Program with GPRbuild
1. Launch a terminal.
2. Use git to clone this repository.
3. Go to the working directory (the newly created `pla-util` directory with the `pla-util.gpr` file).
4. You might need to install gprbuild, on debian: `sudo apt install gprbuild gnat`
5. Run `gprbuild -P pla_util.gpr`.

If the build is successful, the `pla-util` executable will be in the `bin` subdirectory.

## How to Grant the Program Permission to use Packet Sockets
Before you can use the program, you will need to grant the program
permission to use packet sockets. Otherwise, you may see the
following error message when you run the program:

```
pla-util: You don't have permission to capture on that device
```

To grant permission, you can use the following command:

```
sudo setcap cap_net_raw+ep pla-util
```

## How to Run the Program
Running `pla-util --help` displays the following information:

```
pla-util 2.0.0-pre
A utility for power line adapters with Broadcom chipsets

Usage:
  pla-util [options] <command> [arguments]
  pla-util -h | --help                      Display help and exit
  pla-util -V | --version                   Display version and exit

Options:
  -i, --interface=<name>   Network interface to use (e.g., eth0)
  -p, --pla=<mac-address>  Power line adapter at unicast MAC address
  -t, --timeout=<ms>       Network timeout in milliseconds [default: 500]

Commands:
  check-dak <pla-passphrase>         Check device access key
  check-nmk <passphrase>             Check network membership key
  discover                           Discover power line adapters on subnet
  get-capabilities                   Get capabilities
  get-discover-list                  Get discovered PLAs and networks
  get-hfid ( manufacturer | user )   Get human-friendly id [default: user]
  get-id-info                        Get identification info
  get-network-info ( any | member )  Get network information [default: member]
  get-network-stats                  Get average PHY data rates
  reset                              Factory reset power line adapter
  restart                            Restart / reboot power line adapter
  set-hfid <id>                      Set user human-friendly id
  set-nmk <passphrase>               Set network membership key
```

If you don't use the `--interface` option, the program will use the first running, non-loopback network interface that it finds.

If you don't use the `--pla` option, the program will send request packets to the "closest" power line adapter, which is typically an adapter that is accessible via Ethernet cabling
rather than electrical wiring.

## License
This program is licensed under the GNU General Public License Version 3.
