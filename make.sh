#!/bin/sh
gcc -g -nostdlib -fPIC -fPIE -static -Wl,--build-id=none -o arp0 arp0_linux_amd64.s arp0_elf64.s arp0.s
