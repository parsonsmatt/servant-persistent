#! /bin/bash

set -e
echo "Building Perservant..."
stack build
strip `stack exec -- which perservant`
echo "Creating bundle..."
cp `stack exec -- which perservant` perservant
tar -czvf perservant.keter perservant config ql-ui/assets
rm perservant
# scp ./perservant.keter user@host:/opt/keter/incoming/perservant.keter
rm perservant.keter
