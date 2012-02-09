#!/bin/bash

if [ make ]
then
    screen -d -m ssh -R8080:127.0.0.1:8080 db0.fr
    screen -d -m ocsigenserver -c ocsidocs.conf
fi