import sys
import socket

SERVER = 'localhost:8123'
MAX_MESSAGE_SIZE = 1024

def format(message: str):
    return bytes(message + '\r\n', 'ascii')

def check_resp(message: str):
    match message:

def client(tcp_address):
    addr, port = tcp_address.split(':')

    c = socket.socket(socket.AF_INET, socket.SOCK_STREAM) # tcp socket
    c.connect((addr, int(port))) # client connection
    
    while True:
        try:
            msg = input(f'{SERVER}> ')
            c.send(format(msg))

            if msg == 'sair':
                print('saindo....')
                break

            check_resp(msg)

            resp = c.recv(MAX_MESSAGE_SIZE)
            print(resp.decode('utf-8'))
            
        except Exception as e:
            print(e)
            break

    c.close()

def main():
    client(SERVER)


if __name__ == "__main__":
    print('init client')
    main()
