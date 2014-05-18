#!/bin/sh

set -e

STARTDIR=`pwd`

/tmp/beat/bin/beat-0.1.0 stop || echo "No previous node running."
rm -rf /tmp/beat
rm -rf _rel

git checkout v1
make release

git checkout v2
make relup

mkdir -p /tmp/beat
cp _rel/beat/beat-0.1.0.tar.gz /tmp
cp _rel/beat/beat-0.1.1.tar.gz /tmp/beat-0.1.1.tar.gz

cd /tmp/beat
tar xf /tmp/beat-0.1.0.tar.gz

echo "STARTING NODE"
./bin/beat-0.1.0 start

mkdir -p releases/0.1.1
cp /tmp/beat-0.1.1.tar.gz releases/0.1.1/beat.tar.gz

sleep 5s # let the system boot

echo "UPGRADING NODE TO 0.1.1"
./bin/beat-0.1.0 install "0.1.1"

echo "DOWNGRADE NODE TO 0.1.0"
./bin/beat-0.1.0 upgrade "0.1.0"

echo "UPGRADING NODE TO 0.1.1"
./bin/beat-0.1.0 install "0.1.1"

rm /tmp/beat-0.1.0.tar.gz
rm /tmp/beat-0.1.1.tar.gz

cd $STARTDIR
git checkout master
