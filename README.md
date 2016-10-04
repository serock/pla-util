# pla-util
A power line adapter utility

## Overview
The purpose of this program is to allow users to manage DHP-700AV power line adapters from x86-64 Linux.
Two of these adapters are included in the DHP-701AV PowerLine AV2 2000 Gigabit Starter Kit.
DHP-700AV adapters use the BCM60500 chip.

## Development Environment
This program is being developed using:

* [openSUSE Leap 42.1](https://www.opensuse.org/)
* [GNAT GPL Edition](http://libre.adacore.com/) / GNAT Ada 2016, which includes GNAT Programming Studio
* Git 2.6.6

## How to Build the Program with GNAT Programming Studio (GPS)
1. Use git to clone this repository.
2. Launch GPS, select **Open existing project**, and browse to the `pla_util.gpr` project file.
3. Use GPS's **Build All** toolbar button to build the project, or try **Build** > **Project** > **Build All**.

If the build is successful, the `pla-util` executable will be in the same directory as the `pla_util.gpr` project file.

## How to Grant the Program Permission to use Packet Sockets
Before you can use the program, you will need to grant the program
permission to use packet sockets. Otherwise, you may see the
following error messages when you run the program:

```
Execution terminated by unhandled exception
raised ETHERNET.DATAGRAM_SOCKET.SOCKET_ERROR : Operation not permitted
```

To grant permission, you can use the following command:

```
sudo setcap cap_net_raw+ep pla-util
```

## How to Run the Program
To run the program, use one of the following commands:

```
./pla-util <nic> discover-adapters
./pla-util <nic> get-hfid user
./pla-util <nic> get-hfid manufacturer
./pla-util <nic> get-network-info member
./pla-util <nic> get-network-info any
./pla-util <nic> check-dak <plc-pass-phrase>
./pla-util <nic> check-nmk <pass-phrase>
```

where `<nic>` is the name of an ethernet network device (e.g., `eth0` or `enp0s25`)
connected to a DHP-700AV adapter.

## License
This program is licensed under the GNU General Public License Version 3.

## To Do List
* [ ] Implement command to set network membership key (0.7)
* [ ] Implement command to perform factory reset (0.8)
* [ ] Implement command to set user HFID (0.9)
* [ ] Implement command to display adapter info (1.0)

