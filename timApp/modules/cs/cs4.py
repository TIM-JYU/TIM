import BaseHTTPServer
import subprocess
# import nltk
from urllib import urlopen
import re
from urlparse import urlparse,parse_qs
import os.path
import uuid
from os import kill
from signal import alarm, signal, SIGALRM, SIGKILL
from subprocess import PIPE, Popen, check_output
from fileParams import *

PORT=5000

def run_while_true(server_class=BaseHTTPServer.HTTPServer,
                   handler_class=BaseHTTPServer.BaseHTTPRequestHandler):
    """
    This assumes that keep_running() is a function of no arguments which
    is tested initially and after each request.  If its return value
    is true, the server continues.
    """
    server_address = ('', PORT)
    httpd = server_class(server_address, handler_class)
    while keep_running():
        httpd.handle_request()


def generate_filename():
  return str(uuid.uuid4())

def run(args, cwd = None, shell = False, kill_tree = True, timeout = -1, env = None):
  class Alarm(Exception):
          pass




  def alarm_handler(signum, frame):
          raise Alarm

  p = Popen(args, shell = shell, cwd = cwd, stdout = PIPE, stderr = PIPE, env = env)
  if timeout != -1:
          signal(SIGALRM, alarm_handler)
          alarm(timeout)
  try:
          stdout, stderr = p.communicate()
          if timeout != -1:
                  alarm(0)
  except Alarm:
          pids = [p.pid]
          if kill_tree:
                  pids.extend(get_process_children(p.pid))
          for pid in pids:
                  # process might have died before getting to this line
                  # so wrap to avoid OSError: no such process
                  try:
                          kill(pid, SIGKILL)
                  except OSError:
                          pass
          return -9, '', ''
  return p.returncode, stdout, stderr

def get_process_children(pid):
    p = Popen('ps --no-headers -o pid --ppid %d' % pid, shell = True,
              stdout = PIPE, stderr = PIPE)
    stdout, stderr = p.communicate()
    return [int(p) for p in stdout.split()]

def printLines(file,lines,n1,n2):	
	linefmt = "{0:03d} "
	n = len(lines)
	if n1 < 0:  n1 = 0
	if n2 >= n:	n2 = n-1
	
	ni = 0		
	for i in range(n1,n2+1):
		line = lines[i]
		ln = linefmt.format(i+1)
		file.write(ln + line + "\n")
	
	
class TIMServer(BaseHTTPServer.BaseHTTPRequestHandler):
	def do_OPTIONS(self):           
		self.send_response(200, "ok")       
		self.send_header('Access-Control-Allow-Origin', '*')                
		self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
		self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type") 
		print self.path
		print self.headers

	def do_GET(self):
		self.doAll(getParams(self))
		
	def do_POST(self):
		self.doAll(postParams(self))


	def doAll(self,query):	
		print self.path
		print self.headers
		print query
		self.send_response(200)
		# self.send_header('Access-Control-Allow-Origin', '*')
		self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
		self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type") 
		self.send_header('Content-type',    'text/plain') 
		self.end_headers()
		# Generate random cs and exe filenames
		basename = generate_filename()
		csfname = "/tmp/%s.cs" % (basename)
		exename = "/tmp/%s.exe" % (basename)
		p0 = FileParams(query,"","")
		print "p0="
		print p0.replace
		if ( p0.url == "" ):
			self.wfile.write("Must give file= -parameter")
			return
		# Open the file and write it
		csfile = open(csfname, "w")
		p0.printFile(csfile)	
		p0.printInclude(csfile)
		u = p0.url;
		for i in range(1,10):
			p = FileParams(query,str(i),u)
			p.printFile(csfile)	
			p.printInclude(csfile)
			if ( p.url ): u = p.url

		
		csfile.close()
		if not os.path.isfile(csfname):
			self.wfile.write("Could not get the source file\n")
			return
		# Compile
		try:
			#subprocess.check_output(["mcs", "-out:" + exename, csfname], stderr=subprocess.STDOUT, shell=True)
			cmdline = "mcs -out:%s %s" % (exename, csfname)
			check_output([cmdline], stderr=subprocess.STDOUT, shell=True)
			self.wfile.write("*** Success!\n")
		except subprocess.CalledProcessError as e:
			self.wfile.write("!!! Error code " + str(e.returncode) + "\n" )
			self.wfile.write(e.output)
			file = open(csfname, 'r')
			lines = file.read().splitlines()
			# self.wfile.write(file.read())
			printLines(self.wfile,lines,0,10000)
			os.remove(csfname)
			return
        # Execute and write the output
#       try:
#                 cmdline = "mono " + exename
#                 output = subprocess.check_output([cmdline], stderr=subprocess.STDOUT, shell=True)
#                 self.wfile.write(output)
#         except subprocess.CalledProcessError as e:
#                 self.wfile.write("!!! Error code " + str(e.returncode) + "\n" )
#                        self.wfile.write(e.output)
		code, out, err = run(["mono", exename], timeout = 10)
		self.wfile.write(out)
		# Clean up
		os.remove(csfname)
		os.remove(exename)

def keep_running():
  return True

run_while_true(handler_class=TIMServer)

