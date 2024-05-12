import os
import sys
import socket
import hashlib
import pathlib
from multiprocessing import Process

SERVER = 'localhost'
PORT = 8123
CHUNK_SIZE = 8192
MAX_MESSAGE_SIZE = 1024

Clients = []

def create_client():
    c = socket.socket(socket.AF_INET, socket.SOCK_STREAM) # tcp socket
    c.connect((SERVER, PORT)) # client connection
    f = ChunkableFile() # new file
    
    c.send(format('client_id'))
    f.client_id = c.recv(MAX_MESSAGE_SIZE).decode() # get base64 random client id

    return c, f

class ChunkableFile:
    def __init__(self):
        self.client_id = ''
        self.server_file_name = ''
        self.file_name = ''
        self.check_sum = ''
        self.failed = False
        self.file = None
        
    def write(self, data):
        if self.file_name == '':
            self.file_name = pathlib.Path(self.server_file_name).parts[-1]

        if None == self.file:
            self.file = open(f'{self.file_name}.copy', 'w+b') # write read in binary

        self.file.write(data)

    def check_checksum(self):
        self.file.seek(0) # start reading file at the beginning
        hs256 = hashlib.sha256()
        for chunk in iter(lambda: self.file.read(CHUNK_SIZE), b''):
            hs256.update(chunk)

        self.failed = self.check_sum != hs256.hexdigest()

    def close(self):
        if None != self.file and self.failed:
            os.remove(f'{self.file_name}.copy')
            print(f'failed to copy {self.file_name}')
        elif None != self.file:
            print(f'successfully copied {self.server_file_name} to {self.file_name}.copy')
            self.file.close()

    def __repr__(self):
        if None != self.file:
            return self.file
        return self.client_id

# all message sent to server must end with \r\n
def format(message: str):
    return bytes(message + '\r\n', 'ascii')

# read socket and write file
def keep_reading(c, f):
    while True:
        msg = c.recv(CHUNK_SIZE)
        if msg == b'': # stop proccess when server socket was closed
            break
        
        f.write(msg) # write file chunk

    print('')
    f.check_checksum()
    f.close()
    c.close()
    
        
def main():
    c, f = create_client()
    while True:
        try:
            msg = input(f'{f.client_id}@{SERVER}$ ')
            c.send(format(msg))
            resp = c.recv(MAX_MESSAGE_SIZE)

            if resp[-1] == 0xa and msg != 'sair': # catch possible error message
                print(resp.decode(), end='')
                continue

            if msg.startswith('file '):    
                f.server_file_name = msg.split(' ')[1]
                f.check_sum = resp.hex() # should be checksum after file message or error

                p = Process(target=keep_reading, args=(c, f))
                Clients.append(p)
                p.start()
                main() # new client

            if resp.decode().startswith('saindo') or msg == 'sair':
                print('gracefully shutdown')
                f.close()
                c.close()
                break
            
            print(resp.decode()) # common message
            
        except Exception as e:
            print(e)
            break

    for cp in Clients:
        cp.join()


if __name__ == "__main__":
    main()
