#!python3

import signal, threading, subprocess
from socket import AF_INET, SOCK_STREAM, socket
from sys import argv, exit
from time import sleep

COLOR_CODES = {"red": "\033[91m", "blue": "\033[94m", "green": "\033[92m", "clear": "\033[0m"}
PROCS = []

def run_process(command, color_code):
    process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    PROCS.append(process)
    for line in iter(process.stdout.readline, ''):
        print(f"{color_code}{line.strip()}{COLOR_CODES['clear']}")
    process.stdout.close()
    process.wait()

def signal_handler(sig, frame):
    print("\nTerminating processes...")
    for process in PROCS:
        process.terminate()
    exit(0)

def send_add_peer_command(addr_from, addr_to):
  with socket(AF_INET, SOCK_STREAM) as sock:
    sock.connect(("localhost", addr1))
    command = bytearray(2 + len(addr2))
    command[0] = 2 # ADD-PEER command
    command[1] = len(addr2) # len of address
    for i, c in enumerate(addr2):
      command[2 + i] = ord(c)
    sock.sendall(command)
  

if __name__ == "__main__":
  addr1 = int(argv[1][1:]) # node we connect to
  addr2 = argv[2] # node we want to send add-node request to

  commands = [
      "gerbil env pubsub :8000",
      "gerbil env pubsub :8002",
  ]

  signal.signal(signal.SIGINT, signal_handler)

  threads = []
  for command, color_code in zip(commands, ["green", "blue"]):
      thread = threading.Thread(target=run_process, args=(command, COLOR_CODES[color_code]))
      thread.start()
      threads.append(thread)

  sleep(1)
  send_add_peer_command(addr1, addr2)

  for thread in threads:
      thread.join()
