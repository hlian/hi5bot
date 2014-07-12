#!/usr/bin/env python

import subprocess
import sys
import urllib

def curl(s):
    subprocess.call(['curl', s])

def main(user, text):
    curl('http://localhost:2014/?channel_name=ios&user_id=U2147483697&user_name=%s&command=/hi5&text=%s' % (user, text))

main(urllib.quote(sys.argv[1].decode('utf8').encode('utf8')), urllib.quote(sys.argv[2].decode('utf8').encode('utf8')))
