#!/usr/bin/env bash
dir=~/dssat

if [ $# -eq 1 ]
    then
        dir=$1
fi

echo $dir

git clone https://github.com/DSSAT/dssat-csm-os ~/dssat-csm-os --branch v4.8.0.27 && mkdir ~/dssat-csm-os/build && cd ~/dssat-csm-os/build && cmake .. -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=$dir && make
git clone https://github.com/DSSAT/dssat-csm-data ~/dssat-csm-data --branch v4.8.0.27 && cp -r ~/dssat-csm-data/* ~/dssat-csm-os/Data
mkdir $dir/
cp -r ~/dssat-csm-os/Data/* $dir/
cp -r ~/dssat-csm-os/build/bin/* $dir/