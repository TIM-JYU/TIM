from urllib import urlopen
import re
import cgi
import json
from urlparse import urlparse,parse_qs
from pprint import pprint

def get_param(query,key,default):
	if not query.has_key(key): return default
	value = query[key][0]
	if value == 'undefined': return default
	return value
	
def do_matcher(key):
	if not key:
		return False
	return re.compile(key)
		
		
def check(matcher,line):
	if not matcher:
		return False
	match = matcher.search(line)
	return match
		
class FileParams:
	def __init__(self, query, nr,url):
		self.url = get_param(query,"file"+nr,"")
		self.start = do_matcher(get_param(query,"start"+nr,""))
		self.startcnt = int(get_param(query,"startcnt"+nr,"1"))
		self.startn = int(get_param(query,"startn"+nr,"0"))
		self.end = do_matcher(get_param(query,"end"+nr,""))
		self.endcnt = int(get_param(query,"endcnt"+nr,"1"))
		self.endn = int(get_param(query,"endn"+nr,"0"))
		self.linefmt = get_param(query,"linefmt"+nr,"")
		self.maxn = int(get_param(query,"maxn"+nr,"10000"))
		self.lastn = int(get_param(query,"lastn"+nr,"1000000"))
		self.include = get_param(query,"include"+nr,"")
		self.replace = do_matcher(get_param(query,"replace"+nr,""))
		self.by = get_param(query,"by"+nr,"")
		u = get_param(query,"url"+nr,"")
		if ( u and not self.url ): self.url = url
		if ( self.url ): print "url: " + self.url + " " + self.linefmt + "\n"
		
	def	printFile(self,file):
		if ( not self.url ): 
			if ( not self.by ): return
			file.write(self.by.replace("\\n","\n"))
			return
		lines = urlopen(self.url).readlines()    
		# filecontent = nltk.clean_html(html)  
		startcnt = self.startcnt
		endcnt = self.endcnt
		doprint = not ( self.start )
		n  = len(lines)
		n1 = n
		n2 = n
		if ( doprint ):	n1 = 0
		
		for i in range(0,n):
			line = lines[i]
			if not doprint and check(self.start,line): 
				startcnt = startcnt - 1
				# print "startcnt {0} endcnt {1}".format(startcnt,endcnt)
				if ( startcnt <= 0 ):
					doprint = True
					n1 = i
			if doprint and check(self.end,line): 
				endcnt = endcnt - 1
				if ( endcnt <=0 ):
					n2 = i
					break

		n1 += self.startn				
		n2 += self.endn
		if n1 < 0:  n1 = 0
		if n2 >= n:	n2 = n-1
		
		ni = 0		
		for i in range(n1,n2+1):
			line = lines[i]
			if ( check(self.replace,line) ): line = self.by.replace("\\n","\n") + "\n";
			ln = self.linefmt.format(i+1)
			file.write(ln + line)
			if ( i+1 >= self.lastn ): break
			ni = ni + 1
			if ( ni >= self.maxn ):	break
				
	def	printInclude(self,file):
		if ( not self.include ): return
		file.write(self.include.replace("\\n","\n"))


def getParams(self):
	query = parse_qs(urlparse(self.path).query,keep_blank_values=True)
	return query
		
def postParams(self):		
	# print "postParams ================================================"
	# print self		
	# pprint(self.__dict__,indent=2)
	# print dir(self.request)
	form = cgi.FieldStorage(
		fp=self.rfile, 
        headers=self.headers,
        environ={'REQUEST_METHOD':'POST',
                     'CONTENT_TYPE':self.headers['Content-Type'],
                })
	# print self.request.body			
	# print self.rfile	
	# print json.dumps(form)	
	# form = cgi.FieldStorage()
	# print dir(form)
	pprint(form.__dict__, indent=2)
	query = parse_qs(urlparse(self.path).query,keep_blank_values=True)
	# if ( form['type'].find('json') >= 0 ): 
	if ( form.list == None ): # Onko JSON vai tavallinen POST
		s = str(form)
		i = s.find('{')
		i2 = s.rfind('\'')
		js = s[i:i2]
		js = js.replace("\\\\","\\")
		# print js
		jso = json.loads(js)
		# print jso
		for field in jso.keys():
			# print field + ":" + jso[field]
			query[field] = [str(jso[field])]
		return query
	#print form		
	#print "DATA: "
	# print form["data"]
	# print form.keys()
	#print "Environ: "
	#print form.environ	
	for field in form.keys():
		query[field] = [form[field].value]
	return query	
