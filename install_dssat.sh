#!/usr/bin/env bash
git clone https://github.com/DSSAT/dssat-csm-os ~/dssat-csm-os && mkdir ~/dssat-csm-os/build && cd ~/dssat-csm-os/build && cmake .. -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=~/dssat-csm-os/build/bin && make
git clone https://github.com/DSSAT/dssat-csm-data ~/dssat-csm-data && cp -r ~/dssat-csm-data/* ~/dssat-csm-os/Data
cp -r ~/dssat-csm-os/Data/* ~/dssat-csm-os/build/bin