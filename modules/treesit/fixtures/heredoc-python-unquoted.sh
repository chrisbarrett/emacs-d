#!/usr/bin/env bash
python3 <<PYTHON
import os
path = "$HOME/.config"
print(f"Config dir: {path}")
result = os.path.exists(path)
PYTHON
