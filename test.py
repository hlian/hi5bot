#!/usr/bin/env python

import subprocess
import sys

def curl(s):
    subprocess.call(['curl', s])

def main(user, text):
    curl('http://localhost:81/?token=MU0bPLtFTrjbzhipIrueZKb3&channel_name=ios&user_id=U2147483697&user_name=%s&command=/hi5&text=%s' % (user, text))

main(sys.argv[1], sys.argv[2])
