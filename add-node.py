#!python3
from socket import AF_INET, SOCK_STREAM, socket
from sys import argv

if __name__ == "__main__":
  addr1 = int(argv[1][1:]) # node we connect to
  addr2 = argv[2] # node we want to send add-node request to

  with socket(AF_INET, SOCK_STREAM) as sock:
    sock.connect(("localhost", addr1))
    command = bytearray(2 + len(addr2))
    command[0] = 2 # ADD-PEER command
    command[1] = len(addr2) # len of address
    for i, c in enumerate(addr2):
      command[2 + i] = ord(c)
    sock.sendall(command)
