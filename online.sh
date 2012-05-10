#!/bin/bash

./offline.sh

make -C src

if [ $? -eq 0 ]
then
    screen -d -m ssh -R8080:127.0.0.1:8080 db0.fr && screen -d -m ocsigenserver -c ocsidocs.conf
    screen -list
fi
