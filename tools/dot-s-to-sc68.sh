#!/bin/sh

# $1 should end in .s
if [ X`echo $1 | sed -e s/\.s$//` = X$1 ]; then
    echo "The argument to this script ($1) should end in \".s\"."
    exit 1
fi

as68 $1 || exit 1
debug68 <<EOF
ldat `echo $1 | sed -e s/\.s$/.bin/`
r= ymamoto
ft
sd `echo $1 | sed -e s/\.s$/.sc68/`
x
EOF
exit $?
