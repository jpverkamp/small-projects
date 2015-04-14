#!/usr/bin/env python3

def rc4(key, msg):
    if isinstance(key, str): key = key.encode('utf-8')
    if isinstance(msg, str): msg = msg.encode('utf-8')

    S = list(range(256))

    j = 0
    for i in range(256):
        j = (j + S[i] + key[i % len(key)]) % 256
        S[i], S[j] = S[j], S[i]

    def prga():
        i = j = 0
        while True:
            i = (i + 1) % 256
            j = (j + S[i]) % 256
            S[i], S[j] = S[j], S[i]
            yield S[(S[i] + S[j]) % 256]

    return bytes(msgbyte ^ keybyte for msgbyte, keybyte in zip(msg, prga()))
