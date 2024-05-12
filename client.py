import os
import sys
import socket
import hashlib
import multiprocessing

SERVER = 'localhost'
PORT = 8123
MAX_MESSAGE_SIZE = 1024

class ChunkableFile:
    def __init__(self):
        self.client_id = ''
        self.file_name = ''
        self.check_sum = ''
        self.failed = False
        self.file = None
        
    def write(self, data):
        if self.file_name == '':
            return

        if None == self.file:
            self.file = open(f'{self.file_name}.copy', 'w+b') # write read in binary

        self.file.write(bytes(data, 'ascii'))

    def check_checksum(self):
        self.failed = self.check_sum != hashlib.file_digest(self.file, 'sha256').hexdigest()

    def close(self):
        if None != self.file and self.failed:
            os.remove(self.file_name)
        elif None != self.file:
            self.file.close()

    def __repr__(self):
        if None != self.file:
            return self.file
        return ''
    
def format(message: str):
    return bytes(message + '\r\n', 'ascii')

def parse_resp(f: ChunkableFile, message):
    print('message: ', message)
    match message[-1]:
        case 0xa: # common message
            if message.decode('iso-8859-1').startswith(f.file_name):
                f.check_sum = message.decode('iso-8859-1').split(f.file_name)[1]
                #print(f.check_sum)
                return
            
            if message.decode().startswith('saindo'):
                print('gracefully shutdown')
                return

            print(message.decode())
            return

        case _: # anything else should be a filechunk
            
            f.write(message)
            

def client():

    c = socket.socket(socket.AF_INET, socket.SOCK_STREAM) # tcp socket
    c.connect((SERVER, PORT)) # client connection

    f = ChunkableFile()

    c.send(format('client_id'))
    f.client_id = c.recv(MAX_MESSAGE_SIZE).decode('ascii') # get base64 random client id
    
    while True:
        try:
            msg = input(f'{f.client_id}@{SERVER}> ')
            c.send(format(msg))

            if msg.startswith('file '):
                f.file_name = msg.split(' ')[1]
                print(f.file_name)
            
            parse_resp(f, c.recv(MAX_MESSAGE_SIZE))
            
        except Exception as e:
            print(e)
            break
    c.close()

    return f
        
def main():
    f = client()
    print(f)
    f.close()


if __name__ == "__main__":
    main()
