import os
import sys
import socket
import hashlib
import pathlib
from multiprocessing import Process

SERVER = 'localhost'
PORT = 8123
CHUNK_SIZE = 8192

Clients = []

def create_client():
    c = socket.socket(socket.AF_INET, socket.SOCK_STREAM)  # tcp socket
    c.connect((SERVER, PORT))  # client connection
    f = ChunkableFile()  # new file
    
    c.send(format('client_id'))
    f.client_id = c.recv(CHUNK_SIZE).decode()  # get base64 random client id
    return c, f

class ChunkableFile:
    def __init__(self):
        self.client_id = ''
        self.server_file_name = ''
        self.file_name = ''
        self.check_sum = ''
        self.file = None
        self.failed = False

    def write(self, data):
        if self.file_name == '':
            self.file_name = pathlib.Path(self.server_file_name).parts[-1]

        if self.file is None:
            self.file = open(f'{self.file_name}.copy', 'w+b')  # write read in binary

        self.file.write(data)

    def check_checksum(self):
        self.file.seek(0)  # start reading file at the beginning
        hs256 = hashlib.sha256()
        for chunk in iter(lambda: self.file.read(CHUNK_SIZE), b''):
            hs256.update(chunk)

        generated = hs256.hexdigest()

        print(f'verifying checksum... {self.check_sum} X {generated}')
        self.failed = self.check_sum != generated

    def close(self):
        if self.file is not None and self.failed:
            os.remove(f'{self.file_name}.copy')
            print(f'failed to copy {self.file_name}')
        elif self.file is not None:
            print(f'successfully copied {self.server_file_name} to {self.file_name}.copy')
            self.file.close()

    def __repr__(self):
        if self.file is not None:
            return self.file
        return self.client_id

# all message sent to server must end with \r\n
def format(message: str):
    return bytes(message + '\r\n', 'ascii')

# read socket and write file
def keep_reading(c, f):
    while True:
        msg = c.recv(CHUNK_SIZE)
        if msg == b'':  # stop proccess when server socket was closed
            break

        f.write(msg)  # write file chunk

    f.check_checksum()
    f.close()
    c.close()


def main():
    c, f = create_client()
    while True:
        try:
            msg = input(f'{f.client_id}@{SERVER}$ ')
            c.send(format(msg))
            resp = c.recv(CHUNK_SIZE)

            if resp == b'':
                print(f'closing client ${f}')

                return  # return from recursive call

            if resp[-1] == 0xa:  # catch possible error message
                print(resp.decode(), end='')
                continue

            if msg.startswith('file '):
                f.server_file_name = msg.split(' ')[1]
                f.check_sum = resp.hex()  # should be checksum after file message or error

                p = Process(target=keep_reading, args=(c, f))
                Clients.append(p)
                p.start()
                main()  # new client

        except Exception as e:
            print(e)
            break

    c.close()  # only need to close socket connection

    for cp in Clients:
        cp.join()


if __name__ == "__main__":
    main()
