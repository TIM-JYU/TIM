# server to get a file from selected range
# Get parameters:
#   file    = URL for file to get
#   start   = regexp that much match to start printing (default = first line)
#   startcnt= int: how many times the start must match before start (default=1)
#   startn  = int: how many lines to move print forward or backward from start-point (default = 0)  
#   end     = regexp to stop printing (default = last line)
#   endcnt  = int: how many times the end must match before end (default=1)
#   endn    = int: how many lines to move end forward or backward from end-point (default = 0)
#   linefmt = format for line number, f.exe linefmt={0:03d}%20 (default = "") 
#   maxn    = max number of lines to print (default=10000)
#   lastn   = last linenumber to print (default=1000000) 
# Examples
#    ?file=http://example.org/Hello.java&start=main&end=}       -> print from main to first }
#    ?file=http://example.org/Hello.java                        -> print whole file
#    ?file=http://example.org/Hello.java&start=startn=1&endn=-1 -> print file except first and last line
#    ?file=http://example.org/Hello.java&start=main&end=.       -> print only the first line where is main 
#    ?file=http://example.org/Hello.java&start=main&end=.&endn=1  -> print only the first line where is main and next line
#   
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
def get_param(query,key,default):
	if not query.has_key(key):
		return default
	return query[key][0]
def do_matcher(key):
	if not key:
		return False
	return re.compile(key)
def check(matcher,line):
	if not matcher:
		return False
	match = matcher.search(line)
	return match
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
class TIMServer(BaseHTTPServer.BaseHTTPRequestHandler):
	def do_GET(self):
		print self.path
		print self.headers
		query = parse_qs(urlparse(self.path).query,keep_blank_values=True)
		print query
		self.send_response(200)
		self.send_header("Access-Control-Allow-Origin","*")
		self.end_headers()
		# self.wfile.write("kissa")
		# subprocess.call(["svn","export","https://svn.cc.jyu.fi/srv/svn/ohj2/esimerkit/k2014/luennot/live03/src/hello/Hello.java","--force"], shell=False)
		# filecontent = open("Hello.java").read()
		# url = "https://svn.cc.jyu.fi/srv/svn/ohj2/esimerkit/k2014/luennot/live03/src/hello/Hello.java"
		url = get_param(query,"file","")
		start = do_matcher(get_param(query,"start",""))
		startcnt = int(get_param(query,"startcnt","1"))
		startn = int(get_param(query,"startn","0"))
		end = do_matcher(get_param(query,"end",""))
		endcnt = int(get_param(query,"endcnt","1"))
		endn = int(get_param(query,"endn","0"))
		linefmt = get_param(query,"linefmt","")
		maxn = int(get_param(query,"maxn","10000"))
		lastn = int(get_param(query,"lastn","1000000"))
		print "url: " + url + " " + linefmt + "\n"
		# print "startcnt {0} endcnt {1}".format(startcnt,endcnt)
		if ( url == "" ):
			self.wfile.write("Must give file= -parameter")
			return
		lines = urlopen(url).readlines()    
		# filecontent = nltk.clean_html(html)  
		doprint = not ( start )
		n  = len(lines)
		n1 = n
		n2 = n
		if ( doprint ):
			n1 = 0
		for i in range(0,n):
			line = lines[i]
			if not doprint and check(start,line): 
				startcnt = startcnt - 1
				# print "startcnt {0} endcnt {1}".format(startcnt,endcnt)
				if ( startcnt <= 0 ):
					doprint = True
					n1 = i
			if doprint and check(end,line): 
				endcnt = endcnt - 1
				if ( endcnt <=0 ):
					n2 = i
					break
		n1 += startn				
		n2 += endn
		if n1 < 0:
			n1 = 0
		if n2 >= n:
			n2 = n-1
		# Generate random cs and exe filenames
		basename = generate_filename()
		csfname = "/tmp/%s.cs" % (basename)
		exename = "/tmp/%s.exe" % (basename)
		# Open the file and write it
		csfile = open(csfname, "w")
		ni = 0		
		for i in range(n1,n2+1):
			line = lines[i]
			#ln = linefmt.format(i+1)
			#self.wfile.write(ln + line)
			csfile.write(line)
			if ( i+1 >= lastn ):
				break
			ni = ni + 1
			if ( ni >= maxn ):
				break
		# Close the file and check it exists
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
			os.remove(csfname)
			return
		# Execute and write the output
#		try:
#			cmdline = "mono " + exename
#			output = subprocess.check_output([cmdline], stderr=subprocess.STDOUT, shell=True)
#			self.wfile.write(output)
#		except subprocess.CalledProcessError as e:
#			self.wfile.write("!!! Error code " + str(e.returncode) + "\n" )
#                        self.wfile.write(e.output)
		code, out, err = run(["mono", exename], timeout = 10)
		self.wfile.write(out)
		# Clean up
		os.remove(csfname)
		os.remove(exename)
def keep_running():
	return True
run_while_true(handler_class=TIMServer)