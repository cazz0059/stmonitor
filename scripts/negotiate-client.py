#!/usr/bin/env python

CLIENT_HOST = '127.0.0.1'
CLIENT_PORT = 1330

import re, socket
import random, string

MSG_ACCEPT1_RE = re.compile('''^ACCEPT1 ''')
MSG_REJECT1_RE = re.compile('''^REJECT1 ''')
MSG_PROPOSE2_RE = re.compile('''^PROPOSE2 +(.+)''')

if (__name__ == '__main__'):
    print('[C] Client started')
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((CLIENT_HOST, CLIENT_PORT))

    proposal1 = 'PROPOSE1 '
    proposal1 = proposal1 + ''.join(random.choice(string.ascii_uppercase + string.ascii_lowercase + string.digits) for _ in range(20))
    proposal1 = proposal1  + ' 0'
    print(f'[C] Sending: {proposal1}')
    s.sendall(str.encode(proposal1 + '\n'))
    rsp = s.recv(32).decode().strip()
    print('[C] Received:', rsp)

    m = MSG_PROPOSE2_RE.match(rsp)
    if (m is not None):
        #proposal2 = m.group(1)
        #i2 = m.group(2) # didnt work
        req = 'REJECT2 '
        print(f'[C] Sending: {req}')
        s.sendall(str.encode(req + '\n'))

#         rsp = s.recv(32).decode().strip()
#         print('[C] Received:', rsp)
#
#         req = 'RVK ' + tok
#         print(f'[C] Sending: {req}')
#         s.sendall(str.encode(req + '\n'))
