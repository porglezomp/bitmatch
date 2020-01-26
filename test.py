#!/usr/bin/env python3
from contextlib import contextmanager
import subprocess
import os


@contextmanager
def cd(newdir):
    prevdir = os.getcwd()
    os.chdir(os.path.expanduser(newdir))
    try:
        yield
    finally:
        os.chdir(prevdir)


print("Checking no-std compatibility...")
with cd('ensure-no-std'):
    subprocess.check_call(['cargo', 'build'])
