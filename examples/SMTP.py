import smtplib
import sys

def prompt(prompt):
    return raw_input(prompt).strip()

fromaddr = prompt("")
toaddrs  = prompt("").split()

# Add the From: and To: headers at the start!
msg = ("From: %s\r\nTo: %s\r\n\r\n" % (fromaddr, ", ".join(toaddrs)))
while 1:
    try:
        line = raw_input()
    except EOFError:
        break
    msg = msg + line

server = smtplib.SMTP()
server.connect('localhost', 252525)
server.sendmail(fromaddr, toaddrs, msg)
server.quit()
