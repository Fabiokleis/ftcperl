import sys
import socket
import os

SERVER = 'localhost'
PORT = 8123
MAX_MESSAGE_SIZE = 1024

class ChunkableFile:
    def __init__(self):
        self.client_id = ''
        self.file_name = ''
        self.check_sum = ''
        self.failed = False
        self.fo = None
        
    def write(self, data):
        if self.file_name == '':
            return
        
        if None == self.fo:
            self.fo = open(self.file_name, 'w')

        self.fo.write(data)

    def check_check_sum(self, check_sum):
        ...

    def close(self):
        if None != self.fo and self.failed:
            os.remove(self.file_name)
        elif None != self.fo:
            self.fo.close()

    def __repr__(self):
        if None != self.fo:
            return self.fo
        return ''
    
def format(message: str):
    return bytes(message + '\r\n', 'ascii')

def parse_resp(f: ChunkableFile, message: str):
    #print(message)
    match message[-1:]:
        case b'\n': # common message
            print(message.decode(), end='')
        case b'0': # client_id
            f.client_id = message.decode()[:-2]
        case b'1': # filechunk
            f.write_chunk(message[1:])
        case b'2':
            f.check_check_sum(message[1:])
        case _: # anything else
            raise TypeError('unknown message')


def client():

    c = socket.socket(socket.AF_INET, socket.SOCK_STREAM) # tcp socket
    c.connect((SERVER, PORT)) # client connection

    f = ChunkableFile()

    c.send(format('client_id')) # get base64 random client id
    parse_resp(f, c.recv(MAX_MESSAGE_SIZE))
    
    while True:
        try:
            msg = input(f'{f.client_id}@{SERVER}> ')
            c.send(format(msg))

            if msg == 'sair':
                print('saindo....')
                break
            
            resp = c.recv(MAX_MESSAGE_SIZE)
            parse_resp(f, resp)
            
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
