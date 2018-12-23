#!/bin/bash

# File watchers often hit limit
echo 'To fix file watchers reaching the limit, input the below command:'
echo 'echo fs.inotify.max_user_watches=524288 | sudo tee -a /etc/sysctl.conf && sudo sysctl -p'
