import markdown
from flask import Markup
from bs4 import BeautifulSoup
import uuid
import os
import pypandoc

# 
ACCEPTED_TAGS = ['p','h1','h2','h3','h4','h5','em','li','img']

def makeSoup(fileName, srcDir, targetDir):
    ''' Creates a directory in working directory with each paragraph of file with filename
        in their respective files. File is read from srcDir
        and paragraphs are written into files in targetDir.
        A document is created in working directory with name targetDir to mark the order of paragraphs 
        (TODO:running ID's for paragraphs better?)
    '''
    fileCont = open(srcDir+fileName, 'r', encoding="utf-8").read()
    os.makedirs(targetDir)
    soup = BeautifulSoup(fileCont)
    for tag in soup.find_all(True, recursive=False):
        uid = str(uuid.uuid4())
        with open(targetDir + "DOC", 'a') as doc:
            doc.write(uid + "\n")
        with open(os.path.join(targetDir + "/" + '{0}'.format(uid)), 'wb') as f:
            f.write(bytes(str(tag), "UTF-8"))


# import parseParagraphs as p
def test():
    makeSoup("/static/ohj1/raw_lecture_notes.markdown", "./", "./static/ohj1Tags")

def getDocumentPars(docName):
    ''' Returns a table of paragraphs of 
        a given document (docName). These
        can then be formatted in jinja2 template.
    '''
    pars = []
    with open(docName, 'r', encoding="utf-8") as f: 
        for line in f:
            pars.append(line)
    return pars

def parseDocToMarkdown(docName):
    return ""
