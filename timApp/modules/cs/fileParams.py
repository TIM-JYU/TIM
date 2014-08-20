from urllib import urlopen
import re
import cgi
import json
from urlparse import urlparse,parse_qs
from pprint import pprint

class QueryClass:
	query = []
	jso = None

	
def get_param(query,key,default):
	if not query.query.has_key(key): 
		if ( query.jso == None ): return default
		if ( not query.jso.has_key("markup") ): return default
		if ( query.jso["markup"].has_key(key) ): return query.jso["markup"][key]
		return default
	value = query.query[key][0]
	if value == 'undefined': return default
	return value
	
def get_param_del(query,key,default):
	if not query.query.has_key(key): 
		if ( query.jso == None ): return default
		if ( not query.jso.has_key("markup") ): return default
		if ( query.jso["markup"].has_key(key) ): 
			value = query.jso["markup"][key]
			del query.jso["markup"][key]			
			return value
		return default
	value = query.query[key][0]
	del query.query[key]
	if value == 'undefined': return default
	return value
	
def replace_param(query,key,newValue):
	if not query.query.has_key(key): 
		if ( query.jso == None ): return
		if ( not query.jso.has_key("markup") ): return
		if ( query.jso["markup"].has_key(key) ): 
			query.jso["markup"][key] = newValue
		return default
	value = query.query[key][0]
	if value == 'undefined': return
	query.query[key][0] = newValue
	
def do_matcher(key):
	if not key:
		return False
	return re.compile(key)
		
		
def check(matcher,line):
	if not matcher:
		return False
	match = matcher.search(line)
	return match
		

def getJSOParam(jso,key1,key2,default):
	if ( jso == None ): return default
	if ( not jso.has_key(key1) ): return default
	if ( not key2 ): return jso[key1]
	if ( not jso[key1].has_key(key2) ): return default
	return jso[key1][key2];
		
		
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

		usercode = getJSOParam(query.jso,"input"+nr,"usercode",None);
		# if ( query.jso != None and query.jso.has_key("input") and query.jso["input"].has_key("usercode") ):
		if ( usercode ): self.by = usercode
		
		u = get_param(query,"url"+nr,"")
		if ( u and not self.url ): self.url = url
		if ( self.url ): print "url: " + self.url + " " + self.linefmt + "\n"
		
	def	printFile(self,file):
		if ( not self.url ): 
			if ( not self.by ): return
			file.write(self.by.replace("\\n","\n"))
			return
		try:
			lines = urlopen(self.url).readlines()    
		except:
			return
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
		
		replaceBy = self.by
		if ( replaceBy ):
			rep = replaceBy.split("\n");
			if ( len(rep) > 0 and rep[0].strip() == "//" ): 
				del rep[0]
				replaceBy = "\n".join(rep)

		
		for i in range(n1,n2+1):
			line = lines[i]
			if ( check(self.replace,line) ): line = replaceBy + "\n";
			ln = self.linefmt.format(i+1)
			file.write(ln + line)
			if ( i+1 >= self.lastn ): break
			ni = ni + 1
			if ( ni >= self.maxn ):	break
		
		
	def	printInclude(self,file):
		if ( not self.include ): return
		file.write(self.include.replace("\\n","\n"))


def getParams(self):
	result = QueryClass()
	result.query = parse_qs(urlparse(self.path).query,keep_blank_values=True)
	return result

	
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
	result = QueryClass()
	pprint(form.__dict__, indent=2)
	result.query = parse_qs(urlparse(self.path).query,keep_blank_values=True)
	# if ( form['type'].find('json') >= 0 ): 
	if ( form.list == None ): # Onko JSON vai tavallinen POST
		s = str(form)
		i = s.find('{')
		i2 = s.rfind('\'')
		js = s[i:i2]
		js = js.replace("\\\\","\\")
		print "js:======================\n"
		print js
		print "\n======================\n"
		result.jso = json.loads(js)
		# print jso
		for field in result.jso.keys():
			# print field + ":" + jso[field]
			if ( field == "markup" ):
				for f in result.jso[field].keys():
					result.query[f] = [str(result.jso[field][f])]
			else:
				if ( field != "state" ): result.query[field] = [str(result.jso[field])]
		return result
	#print form		
	#print "DATA: "
	# print form["data"]
	# print form.keys()
	#print "Environ: "
	#print form.environ	
	for field in form.keys():
		result.query[field] = [form[field].value]
	return result
