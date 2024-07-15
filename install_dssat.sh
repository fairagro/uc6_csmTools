#!/usr/bin/env bash
git clone https://github.com/DSSAT/dssat-csm-os ~/dssat-csm-os --branch v4.8.0.27 && mkdir ~/dssat-csm-os/build && cd ~/dssat-csm-os/build && cmake .. -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=~/dssat && make
git clone https://github.com/DSSAT/dssat-csm-data ~/dssat-csm-data --branch v4.8.0.27 && cp -r ~/dssat-csm-data/* ~/dssat-csm-os/Data
mkdir ~/dssat/
cp -r ~/dssat-csm-os/Data/* ~/dssat/
cp -r ~/dssat-csm-os/build/bin/* ~/dssat/