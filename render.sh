#!/bin/bash
#
# Purpose: Render website in container
# Author: Valentin Ziel
#

quarto render
cp -r docs/* website/
