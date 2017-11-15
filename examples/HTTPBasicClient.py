import httplib
conn = httplib.HTTPConnection("localhost:8081")
conn.request("GET", "/hello")
