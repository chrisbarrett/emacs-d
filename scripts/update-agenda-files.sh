#!/usr/bin/env bash

set -e -o pipefail

org_dir=${ORG_DIRECTORY:-$HOME/org}

cd "$org_dir"

{
    echo notes.org
    rg --follow --files-with-matches '^(CLOCK:|[*]+ +(TODO|WAIT))' roam -g '!attach' || true
} >"$org_dir"/org-agenda-files
