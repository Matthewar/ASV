#!/usr/bin/python
import os
import subprocess
import sys

cwd = os.getcwd()
newpath = subprocess.check_output(["stack","path","--local-doc-root"]).strip("\n\r\f")

sys.stdout.write(os.path.relpath(newpath,cwd))
sys.stdout.flush()
