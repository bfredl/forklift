#!/bin/bash
mount -n -t proc proc /proc
mount -n -t sysfs sysfs /sys
mount -o bind /usr/lib/modules/uml /lib/modules
mount -n -t tmpfs tmpfs /tmp -o rw,nosuid,nodev

exec setsid bash < /dev/tty0 &> /dev/tty0
