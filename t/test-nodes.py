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

def make_cmd(command, msg):
  cmd = bytearray(2 + len(msg))
  cmd[0] = command
  cmd[1] = len(msg)
  for i, c in enumerate(msg):
    cmd[2 + i] = ord(c)

  return cmd

def greet_node(sock):
  cmd = make_cmd(3, "python script")
  sock.sendall(cmd)

def populate_node(sock):
  sock.sendall(make_cmd(1, "kek"))
  sock.sendall(make_cmd(1, "lol"))

def add_peer(sock, addr_to):
  sock.sendall(make_cmd(2, addr_to))
  

if __name__ == "__main__":
  addr1 = argv[1] # node we connect to
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
  with socket(AF_INET, SOCK_STREAM) as sock1:
    with socket(AF_INET, SOCK_STREAM) as sock2:
      sock1.connect(("localhost", int(addr1[1:])))
      sock2.connect(("localhost", int(addr2[1:])))
      greet_node(sock1)
      greet_node(sock2)
      populate_node(sock1)
      populate_node(sock2)
      add_peer(sock1, addr2)

  for thread in threads:
      thread.join()
