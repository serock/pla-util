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

## Binary Packages
Binary packages and repositories for the latest `pla-util` release are available for multiple Linux distributions and architectures.
Instructions for adding repositories and installing binary packages can be found on the [download](https://software.opensuse.org/download.html?project=home%3Aserock&package=pla-util) page.

For additional information, see the [Binary Packages](../../wiki/Binary-Packages) page on the wiki.

## Development Environment
The following software is being used to develop `pla-util` on [openSUSE Leap](https://www.opensuse.org/):

* GNU Ada Compiler (GNAT)
* GNU Make
* GPRbuild
* [Alire](https://alire.ada.dev/)
* [GNAT Studio](https://github.com/AdaCore/gnatstudio/releases)
* Git
* [libpcap](https://www.tcpdump.org/)

If your Linux distribution does not provide a package for GPRbuild, you can use `make` and `gnatmake` to build.
Another option is to use Alire's `alr` command-line tool, which will download a GNU Ada Compiler (GNAT) and GPRbuild when `alr` builds a project for the first time.

### Hardware
During development, `pla-util` is tested with D-Link DHP-700AV adapters from a *D-Link DHP-701AV PowerLine AV2 2000 Gigabit Starter Kit*.

### Software
The `libpcap` package(s) are required to build and run `pla-util`.

:information_source: **Note:**

* See the [Dependencies](../../wiki/Dependencies) page on the wiki for software packages that should be installed before attempting to build.

## How to Build the Program with make and gnatmake
1. Use git to clone this repository.
2. In a terminal, go to the working directory (the newly created `pla-util` directory with the `Makefile` file).
3. Run `make`.

If the build is successful, the `pla-util` executable will be in the `bin` subdirectory.

## How to Build the Program with GPRbuild
1. Use git to clone this repository.
2. In a terminal, go to the working directory (the newly created `pla-util` directory with the `pla-util.gpr` file).
3. Run `gprbuild -P pla_util.gpr`. However, if the build fails with _object directory "obj" not found_ messages, retry the build by running `gprbuild -p -P pla_util.gpr`.

If the build is successful, the `pla-util` executable will be in the `bin` subdirectory.

## How to Build the Program with Alire
1. Use git to clone this repository.
2. In a terminal, go to the working directory (the newly created `pla-util` directory with the `pla-util.gpr` file).
3. Run `alr build`.

If the build is successful, the `pla-util` executable will be in the `bin` subdirectory.

## How to Build the Program with GNAT Studio
1. Use git to clone this repository.
2. Launch `gnatstudio`, select **Open existing project**, and browse to the `pla_util.gpr` project file.
3. Use GNAT Studio's **Build All** toolbar button to build the project, or try **Build** > **Project** > **Build All**.

If the build is successful, the `pla-util` executable will be in the `bin` subdirectory.

## How to Build a Docker Image
1. Use git to clone this repository.
2. In a terminal, go to the working directory (the newly created `pla-util` directory with the `pla-util.gpr` file).
3. Run `docker build -t pla-util .`.

If the build is successful, the new `pla-util` minimal runtime image can be run with Docker.
For example,

```
docker run -i -t --network=host pla-util
```

## How to Grant the Program Permission to use Packet Sockets
If you do not intend to run `pla-util` as root, then before you can use the program, you will need to grant `pla-util` permission to use packet sockets.
Otherwise, you may see the following error message when you run the program:

```
pla-util: You don't have permission to capture on that device
```

Use the `filecap` command to set the `net_raw` capability on `pla-util`. 
If `pla-util` was installed in `/usr/bin`, you can use the following command:

```
sudo filecap /usr/bin/pla-util net_raw
```

## How to Run the Program
Running `pla-util --help` displays the following information:

```
pla-util 2.1.0
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
  get-station-info                   Get power line adapter information
  reset                              Factory reset power line adapter
  restart                            Restart / reboot power line adapter
  set-hfid <id>                      Set user human-friendly id
  set-nmk <passphrase>               Set network membership key
```

If you don't use the `--interface` option, the program will use the first running, non-loopback network interface that it finds.

If you don't use the `--pla` option, the program will send request packets to the "closest" power line adapter, which is typically an adapter that is accessible via Ethernet cabling
rather than electrical wiring.

## Bash Completion
A bash completion file for the `pla-util` command is available at `completions/pla-util` in this project.
The `pla-util` completion file depends on the `bash-completion` package, which is installed by default in many Linux distributions.
As noted in the `bash-completion` [FAQ](https://github.com/scop/bash-completion/#faq), the completion file can be put into in one of the following directories:

* `$BASH_COMPLETION_USER_DIR/completions`
* `$XDG_DATA_HOME/bash-completion/completions`, if `BASH_COMPLETION_USER_DIR` is not set
* `~/.local/share/bash-completion/completions`, if `BASH_COMPLETION_USER_DIR` and `XDG_DATA_HOME` are not set

:information_source: **Note:**

1. After the 2.1.0 release, the bash completion file was renamed from `pla-util.bash` to `pla-util`.
2. If you download and install a binary package for the 2.1.0 release or later, the bash completion file is installed as `/usr/share/bash-completion/completions/pla-util`.

## License
This program is licensed under the GNU General Public License Version 3 or later.
