# Echo server program
import socket

HOST = 'localhost'
PORT = 10001
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((HOST, PORT))
s.listen(1)
conn, addr = s.accept()
data = conn.recv(1024)
conn.sendall(data)
conn.close()
print '-'
