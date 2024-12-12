#!/usr/bin/env bash
#
# Purpose: Render website in container
# Author: Valentin Ziel
# Folder check from https://stackoverflow.com/questions/91368/checking-from-shell-script-if-a-directory-contains-files
# 

# when a command fails, bash exits instead of continuing with the rest of the script.
set -o errexit
set -e

WEBSITE_FOLDER="/app/website/"

# -------------------------------------------------------------
echo "start rendering website files (html default)"
quarto render 

# Check if the quarto render command succeeded
if [ $? -eq 0 ]; then
    echo "Quarto render succeeded" 
    echo "Copy new files to webfolder destination"
else
    echo "Quarto render failed"
    exit 1
fi

# -------------------------------------------------------------
files=$(shopt -s nullglob dotglob; echo $WEBSITE_FOLDER*)

# -------------------------------------------------------------
if (( ${#files} ))
then
  echo "website  folder contains files - removing"
  rm -rf $WEBSITE_FOLDER*
  echo "removed old files"
else 
  echo "website folder empty (or does not exist or is a file)"
fi

# -------------------------------------------------------------
echo "copy new files"
cp -r docs/* $WEBSITE_FOLDER

# -------------------------------------------------------------
echo "chmod r+x website files"
chmod -R +rx  $WEBSITE_FOLDER

# -------------------------------------------------------------
echo "done"
