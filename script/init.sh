#!/bin/bash
set -e
bundle
npm install -g grunt-cli bower
npm install
bower install
grunt
