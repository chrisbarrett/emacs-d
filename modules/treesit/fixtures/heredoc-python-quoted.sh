#!/usr/bin/env bash
python3 <<'PYTHON'
def greet(name: str) -> str:
    """Return greeting for NAME."""
    return f"Hello, {name}!"

class Greeter:
    def __init__(self, prefix="Hi"):
        self.prefix = prefix
PYTHON
