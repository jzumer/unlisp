#!/bin/sh
gcc -static -nostdlib -fPIC -fPIE -Wl,--build-id=none -o arp0 arp0_assembly.s arp0_linux_amd64.s arp0_elf64_static.s arp0.s
