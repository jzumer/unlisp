#!/bin/sh
gcc -g -nostdlib -fPIC -fPIE -Wl,--build-id=none -o ul0 ul0_assembly.s ul0_linux_amd64.s ul0_elf64_static.s ul0.s
