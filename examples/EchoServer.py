# Echo server program
import socket

HOST = 'localhost'  # Symbolic name meaning all available interfaces
PORT = 10001        # Arbitrary non-privileged port
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((HOST, PORT))
s.listen(1)
conn, addr = s.accept()
data = conn.recv(1024)
conn.sendall(data)
conn.close()
print '-'
