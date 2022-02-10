# pla-util
A power line adapter utility

## Overview
The purpose of this program is to allow Linux users to manage HomePlug AV2 compliant power line adapters that use a Broadcom system-on-a-chip (SoC) such as the BCM60500 or BCM60333.

### Compatible Power Line Adapters
This program has been used with the following power line adapters:

* D-Link DHP-700AV (uses BCM60500)
* NETGEAR PLP1000 (uses BCM60333)
* NETGEAR PLP2000 (uses BCM60500)
* Tenda PH3 (uses BCM60355)
* TP-Link TL-PA7017 (uses BCM60355)
* TP-Link TL-PA9020 (uses BCM60500)

## Development Environment
This program is being developed using:

* [openSUSE Leap](https://www.opensuse.org/)
* [GNAT Community Edition](https://www.adacore.com/download), which includes the GNAT Studio IDE and the GPRbuild tool
* [Git](https://git-scm.com/)

### Test Hardware
During development, this program is tested on D-Link DHP-700AV adapters from a D-Link DHP-701AV PowerLine AV2 2000 Gigabit Starter Kit.

## How to Build the Program with GNAT Studio
1. Use git to clone this repository.
2. Launch `gnatstudio`, select **Open existing project**, and browse to the `pla_util.gpr` project file.
3. Use GNAT Studio's **Build All** toolbar button to build the project, or try **Build** > **Project** > **Build All**.

If the build is successful, the `pla-util` executable will be in the same directory as the `pla_util.gpr` project file.

## How to Build the Program with GPRbuild
1. Launch a terminal.
2. Use git to clone this repository.
3. Go to the working directory (the newly created `pla-util` directory with the `pla-util.gpr` file).
4. You might need to install gprbuild, on debian: `sudo apt install gprbuild gnat`
5. Run `gprbuild -P pla_util.gpr`.

If the build is successful, the `pla-util` executable will be in the same directory as the `pla_util.gpr` project file.

## How to Grant the Program Permission to use Packet Sockets
Before you can use the program, you will need to grant the program
permission to use packet sockets. Otherwise, you may see the
following error message when you run the program:

```
Error: Operation not permitted
```

To grant permission, you can use the following command:

```
sudo setcap cap_net_raw+ep pla-util
```

## How to Run the Program
To run the program, use one of the following commands:

```
./pla-util <nic> discover
./pla-util <nic> reset <pla-mac-address>
./pla-util <nic> restart <pla-mac-address>
./pla-util <nic> get-hfid manufacturer
./pla-util <nic> get-hfid user
./pla-util <nic> get-id-info
./pla-util <nic> get-network-info member
./pla-util <nic> get-network-info any
./pla-util <nic> get-network-stats
./pla-util <nic> set-hfid <id>
./pla-util <nic> set-nmk <pass-phrase>
./pla-util <nic> check-dak <plc-pass-phrase>
./pla-util <nic> check-nmk <pass-phrase>
```

where
`<nic>` is the name of an ethernet network device (e.g., `eth0` or `enp0s25`)
connected to a power line adapter.
`<pla-mac-address>` is the MAC address of a power line adapter as reported by the `discover` command.

## License
This program is licensed under the GNU General Public License Version 3.


