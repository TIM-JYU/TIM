import BaseHTTPServer
import subprocess
# import nltk
from urllib import urlopen,urlencode
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

def printFileTo(name, f):	
	fr = open(name, "r")
	lines = fr.readlines()    
	for i in range(0,len(lines)):
		line = lines[i]
		f.write(line)
	fr.close()
	
def queryParamsToNG(query):
	result = "" 
	for field in query.keys():
		result = result + field + "=\"" + query[field][0] + "\";\n"
	# print "QUERY" + str(query)
	return result
	
def queryParamsToMap(query):
	result = {}
	for field in query.keys():
		result[field] = query[field][0]
	return result
	
def printFileToReplaceNG(name, f, whatToReplace, query):	
	fr = open(name, "r")
	lines = fr.readlines()    
	params = queryParamsToNG(query)
	for i in range(0,len(lines)):
		line = lines[i].replace(whatToReplace,params)
		f.write(line)
	fr.close()
	
def printFileToReplaceURL(name, f, whatToReplace, query):	
	fr = open(name, "r")
	lines = fr.readlines()    
	# params = queryParamsToURL(query)
	map = queryParamsToMap(query)
	params = urlencode(map)
	for i in range(0,len(lines)):
		line = lines[i].replace(whatToReplace,params)
		f.write(line)
	fr.close()
	
	
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
		print "do_OPTIONS =============================================="
		self.send_response(200, "ok")       
		self.send_header('Access-Control-Allow-Origin', '*')                
		self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
		self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type") 
		print self.path
		print self.headers

	def do_GET(self):
		print "do_GET =================================================="
		self.doAll(getParams(self))
		
	def do_POST(self):
		print "do_POST ================================================="
		self.doAll(postParams(self))


	def doAll(self,query):	
		print "doAll ==================================================="
		print self.path
		print self.headers
		print query
		
		fullhtml = self.path.find('/fullhtml') >= 0
		css = self.path.find('/css') >= 0 
		js = self.path.find('/js') >= 0 
		iframe = self.path.find('/iframe') >= 0 
		
		self.send_response(200)
		# self.send_header('Access-Control-Allow-Origin', '*')
		self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
		self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type") 
		type = 'text/plain'
		if ( fullhtml ): type = 'text/html'
		if ( css ): type = 'text/css'
   		if ( js ) : type = 'application/javascript' 
		self.send_header('Content-type',type) 
		self.end_headers()
		# Get the template type
		ttype = get_param(query, "type", "console").lower()

		if ( css ):
			printFileTo('cs.css',self.wfile)
			return
		if ( js ):
			printFileTo('aja.js',self.wfile)
			return
		if ( fullhtml ):
			printFileTo('begin.html',self.wfile)
			if ( ttype == "console" ):
				printFileToReplaceNG('consoleTemplate.html',self.wfile,"##QUERYPARAMS##",query)
			else:	
				printFileToReplaceNG('jypeliTemplate.html',self.wfile,"##QUERYPARAMS##",query)
			printFileTo('end.html',self.wfile)
			return
		if ( iframe ):
			printFileToReplaceURL('iframeTemplate.html',self.wfile,"##QUERYPARAMS##",query)
			return

		# Generate random cs and exe filenames
		basename = generate_filename()
		csfname = "/tmp/%s.cs" % (basename)
		exename = "/tmp/%s.exe" % (basename)

		# Check query parameters
		p0 = FileParams(query,"","")
		print "p0="
		print p0.replace
		if ( p0.url == "" and p0.replace == "" ):
			self.wfile.write("Must give file= -parameter")
			return

		printFile = get_param(query, "print", "")
		print "type=" + ttype

		if ( ttype == "console" ):
			# Console program
			pass
		elif ( ttype == "jypeli" ):
			# Jypeli game
			bmpname = "/tmp/%s.bmp" % (basename)
			pngname = "/cs/images/%s.png" % (basename)
			pass
		else:
			# Unknown template
			self.wfile.write("Invalid project type given (type=" + ttype + ")")
			return

		# Open the file and write it
		if ( printFile ): csfile = self.wfile
		else: csfile = open(csfname, "w")
		p0.printFile(csfile)	
		p0.printInclude(csfile)
		u = p0.url;
		for i in range(1,10):
			p = FileParams(query,str(i),u)
			p.printFile(csfile)	
			p.printInclude(csfile)
			if ( p.url ): u = p.url
			
		if ( printFile ): return

		
		csfile.close()
		if not os.path.isfile(csfname):
			self.wfile.write("Could not get the source file\n")
			print "=== Could not get the source file"
			return

		# Compile
		try:
			if ( ttype == "jypeli" ):
				cmdline = "mcs /out:%s /r:/cs/jypeli/Jypeli.dll /r:/cs/jypeli/Jypeli.MonoGame.Framework.dll /r:/cs/jypeli/Jypeli.Physics2d.dll /r:/cs/jypeli/OpenTK.dll /r:/cs/jypeli/Tao.Sdl.dll /r:System.Drawing /cs/jypeli/Ohjelma.cs /cs/jypeli/Screencap.cs %s" % (exename, csfname)
			else:
				cmdline = "mcs /out:%s %s" % (exename, csfname)

			check_output([cmdline], stderr=subprocess.STDOUT, shell=True)
			self.wfile.write("*** Success!\n")
			print "*** Success"
		except subprocess.CalledProcessError as e:
			self.wfile.write("!!! Error code " + str(e.returncode) + "\n" )
			self.wfile.write(e.output)
			file = open(csfname, 'r')
			lines = file.read().splitlines()
			# self.wfile.write(file.read())
			printLines(self.wfile,lines,0,10000)
			os.remove(csfname)
			print "!!! Error code " + str(e.returncode) + "\n" 
			return


		if ( ttype == "jypeli" ):
			code, out, err = run(["mono", exename, bmpname], timeout = 10)
			run(["convert", "-flip", bmpname, pngname])
			os.remove(bmpname)
			self.wfile.write("*** Screenshot: http://tim-beta.it.jyu.fi/csimages/%s.png\n" % (basename))
			print ("*** Screenshot: http://tim-beta.it.jyu.fi/csimages/%s.png\n" % (basename))
			# TODO: clean up screenshot directory
		else:
			 code, out, err = run(["mono", exename], timeout = 10)

		self.wfile.write(out)
		self.wfile.write(err)
		print out
		print err
		
		# Clean up
		os.remove(csfname)
		os.remove(exename)

def keep_running():
  return True

run_while_true(handler_class=TIMServer)

