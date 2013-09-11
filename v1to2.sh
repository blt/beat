#!/bin/sh

set -e

rm -rf /tmp/beat
rm -rf _rel

git checkout v1
make release

git checkout v2
make relup

mkdir -p /tmp/beat
cp _rel/beat-0.1.0.tar.gz /tmp
cp _rel/beat-0.1.1.tar.gz /tmp/beat-0.1.1.tar.gz

cd /tmp/beat
tar xf /tmp/beat-0.1.0.tar.gz
./bin/beat-0.1.0 start

mkdir -p releases/0.1.1
cp /tmp/beat-0.1.1.tar.gz releases/0.1.1/beat.tar.gz

rm /tmp/beat-0.1.0.tar.gz
rm /tmp/beat-0.1.1.tar.gz
