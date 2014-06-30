#!/bin/sh

#  prebuild.sh
#  Tapestry
#
#  Created by Ira on 6/30/14.
#  Copyright (c) 2014 Ira. All rights reserved.
rm -rf Tapestry/www/src/
mkdir Tapestry/www/src/
cp -rf ../../../../apps/tapestry/priv/www/ Tapestry/www/src/
