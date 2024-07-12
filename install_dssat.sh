#!/usr/bin/env bash
git clone https://github.com/DSSAT/dssat-csm-os ~/dssat-csm-os && mkdir ~/dssat-csm-os/build && cd ~/dssat-csm-os/build && cmake .. && make
git clone https://github.com/DSSAT/dssat-csm-data ~/dssat-csm-data && cp -r ~/dssat-csm-data/* ~/dssat-csm-os/Data