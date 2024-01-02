#!/bin/bash
#
# Purpose: Render website in container
# Author: Valentin Ziel
#

quarto render index.qmd
cp -r docs/* website/
