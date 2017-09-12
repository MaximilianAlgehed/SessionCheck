# Echo server program
import socket

HOST = 'localhost'
PORT = 10002
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((HOST, PORT))
s.listen(1)
while True:
    conn, addr = s.accept()
    data = conn.recv(1024)
    if data == "\r\n":
        conn.sendall("bob\r\n")
    else:
        conn.sendall(data)
