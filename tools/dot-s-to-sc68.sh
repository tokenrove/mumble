#!/bin/sh
#
# Wishlist:
# - extract title, composer, etc, put into sc68 file properly.
#
# Julian Squires <tek@wiw.org> / 2004

# $1 should end in .s
if [ X`echo $1 | sed -e s/\.s$//` = X$1 ]; then
    echo "The argument to this script ($1) should end in \".s\"."
    exit 1
fi
binfile=`echo $1 | sed -e s/\.s$/.bin/`
sc68file=`echo $1 | sed -e s/\.s$/.sc68/`

as68 $1 || exit 1
debug68 <<EOF
ldat $binfile
r= ymamoto
ft
sd $sc68file
x
EOF
if [ $? -eq 0 ]; then
    rm $binfile
else
    exit $?
fi
