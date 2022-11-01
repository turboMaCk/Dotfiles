#!/usr/bin/env bash

set -ex

git pull
git submodule update --init --recursive
