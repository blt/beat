#!/bin/sh

set -e

rm -rf /tmp/beat
rm -rf _rel

git checkout v1
make release
tar cvzf beat-0.1.0.tar.gz -C _rel .

git checkout v2
make relup
tar cvzf beat-0.1.1.tar.gz -C _rel .

mkdir -p /tmp/beat
cp _rel/releases/beat-0.1.1/beat.rel /tmp/
cp beat-0.1.0.tar.gz /tmp
cp beat-0.1.1.tar.gz /tmp

cd /tmp/beat
tar xf /tmp/beat-0.1.0.tar.gz
./bin/beat-0.1.0 start

mkdir releases/beat-0.1.1
cp /tmp/beat-0.1.1.tar.gz releases/beat-0.1.1/
cp /tmp/beat.rel releases/beat-0.1.1/beat-0.1.1.rel

rm /tmp/beat.rel
rm /tmp/beat-0.1.0.tar.gz
rm /tmp/beat-0.1.1.tar.gz
