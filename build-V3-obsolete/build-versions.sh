#!/bin/bash

# What to build
export VE_SKIP_SRC_INSTALLER=no # unset VE_SKIP_SRC_INSTALLER and comment out to not build the source installer
Rversions=( "4.1.3" "4.2.3" "4.3.3" "4.4.2" )
export VE_MAKE_TARGETS="all installer"
# export VE_MAKE_TARGETS="configure show-defaults"

echo -n Building Targets: ""
for version in "${Rversions[@]}"; do
  echo -n "$version, "
done
echo

echo Building in \"${VE_BUILD:=N:/Git-Repos/VisionEval-built-4.0}\"  # If VE_BUILD is not set or null, set its value to 'default'.
export VE_BUILD

echo Building Branch \"${VE_BRANCH:=$(git rev-parse --abbrev-ref HEAD)}\"
export VE_SKIP_SRC_INSTALLER
export VE_BUILD
export VE_BRANCH

# Make the target directories
# mkdir -p says create recursively, but just smile silently if they already exist
mkdir -p $VE_BUILD 
mkdir -p $VE_BUILD/dev/logs/$VE_BRANCH # so there is some place to put MAKE_OUT

set +m # turn off shell "monitor mode" so no message appears from the shell when make process finishes
for version in "${Rversions[@]}"; do
  export VE_R_VERSION=${version}
  export MAKE_OUT=$VE_BUILD/dev/logs/$VE_BRANCH/make.$VE_R_VERSION.out
  make ${VE_MAKE_TARGETS} >$MAKE_OUT 2>&1 &
  export MAKE_PID=$!
  echo ===================================================================
  echo "Making '${VE_BRANCH}' for '${VE_R_VERSION}' as Process [$MAKE_PID]"
  echo
  tail -f --pid=$MAKE_PID $MAKE_OUT
  echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  echo
done

echo All builds of ${VE_BRANCH} are Complete
