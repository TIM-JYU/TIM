# import redis
from flask import Flask, request, make_response
from libvoikko import Voikko, Sentence, GrammarError, Token
from html.parser import HTMLParser
import json

# Instance of Flask framework
app = Flask(__name__)

# Redis
# cache = redis.Redis(host='redis', port=6379)

# Instance of Voikko (Linquistic tool for Finnish)
v = Voikko("fi")

'''
Luokka, jonka jäsenfunktioissa käsitellään html:n parsiminen
Vähän toiminnallisuudesta:
Luokka rakentaa alkuperäisen html:n uudelleen (jäsenmuuttuja html) iteroimalla
alkuperäistä html:ää samalla merkiten mahdolliset virheet iteroinnin varrella.
Muutkin lähestymistavat mahdollisia.
'''


class HtmlParser(HTMLParser):
    def __init__(self):
        HTMLParser.__init__(self)
        self.currentTag = ''
        self.currentClass = ''
        self.bannedTags = ['code']
        self.bannedClasses = ['math inline', 'math display', 'mathp inline', 'mathp display', 'nospell']
        self.words = []
        self.html = ''
        self.errors = 0
        self.line = 1
        self.position = 0

    # Syötetään html yläluokalle parsittavaksi
    # def feed(self, data):
    #    self.words = []
    #    self.currentTag = ''
    #    self.currentClass = ''
    #    self.html = ''
    #    self.errors = 0
    #    self.line = 1
    #    self.position = 0

    #    try:
    #        HTMLParser.feed(self, data)
    #    except AssertionError:
    #        print("AssertionError")
    #        self.words = []
    #        self.html = ''
    #        return self.words, self.html

    #    return self.words, self.html

    # Aloitustägeihin liittyvä toiminnallisuus
    def handle_starttag(self, tag, attrs):
        self.currentTag = tag
        attrdata = ''
        for attr in attrs:
            if attr[0] == 'class':
                self.currentClass = attr[1]
            attrdata += f' {attr[0]}=\"{attr[1]}\"'

        fulltag = f'<{tag}{attrdata}>'
        self.html += fulltag
        self.position += (len(fulltag))

    # Lopetustägeihin liittyvä toiminnallisuus
    def handle_endtag(self, tag):
        self.currentTag = ''
        self.currentClass = ''
        self.html += str('</' + tag + '>')
        self.position += (len(tag)+3)

    # Varsinaiseen tekstisisältöön liittyvä toiminnallisuus
    def handle_data(self, data):
        startpos = self.getpos()[1]
        currentpos = startpos
        if data == '\n':
            self.html += data
            self.position += len(data)
            return
        else:
            spell_checked_words = []
            tokenized_words = v.tokens(data)
            is_style = False
            previous_word = ""
            for word in tokenized_words:
                checked_word = {}
                if word.tokenType == Token.WORD and v.spell(word.tokenText) is False and is_style is False:
                    self.errors += 1
                    checked_word['word'] = word.tokenText
                    checked_word['id'] = self.errors
                    checked_word['suggestions'] = v.suggest(word.tokenText)
                    checked_word['error'] = True
                    spell_checked_words.append(checked_word)
                else:
                    if previous_word == "{" and word.tokenText == ".":
                        is_style = True
                    elif word.tokenText == "}":
                        is_style = False
                    checked_word['word'] = word.tokenText
                    checked_word['error'] = False
                    spell_checked_words.append(checked_word)
                previous_word = word.tokenText

            for word in spell_checked_words:
                newword = word
                newword['position'] = self.position
                currentlength = len(word['word'])
                newword['length'] = currentlength
                currentpos += currentlength
                self.position += currentlength
                if word['error'] and self.currentTag not in self.bannedTags and self.currentClass not in self.bannedClasses:
                    self.words.append(newword)
                    self.html += f"<spell-error style=\"pointer-events: auto; cursor: pointer;\" wordid=\"{newword['id']}\" text=\"{newword['word']}\"></spell-error>"
                else:
                    self.html += f"{newword['word']}"




def spell_check_words(query_string):
    '''
    Iterates through words and for every incorrect spelled word, it returns a list of 
    dictionaries that contains:
    The word
    List of suggestions to replace the word
    '''

    spell_checked_words = []

    #Mitä tämä allaoleva kohta tykkää html-elementeistä?
    tokenized_words = v.tokens(query_string)

    for word in tokenized_words:
        if word.tokenType == Token.WORD and v.spell(word.tokenText) is False:
            checked_word = {}
            checked_word['word'] = word.tokenText
            checked_word['suggestions'] = v.suggest(word.tokenText)
            spell_checked_words.append(checked_word)
    return spell_checked_words


def grammar_check_sentences(query_string):
    '''
    Iterates sentences and for every sentence with grammar error, it returns a list 
    of dictionaries that includes:
    The sentence
    List of grammar errors.
    '''

    sentences = v.sentences(query_string)
    grammar_checked_sentences = []
    for sentence in sentences:
        grammar_error_objects = v.grammarErrors(sentence.sentenceText, 'fi')
        if grammar_error_objects != []:
            grammar_checked_sentence = {}
            grammar_checked_sentence['sentence'] = sentence.sentenceText
            grammar_checked_sentence['grammar_errors'] = []
            for grammar_error_object in grammar_error_objects:
                #TODO: Allaoleva esitys lauseen virheistä ei ole toimiva, vaatii paremman ratkaisun
                grammar_checked_sentence['grammar_errors'].append(grammar_error_object.toString())
            grammar_checked_sentences.append(grammar_checked_sentence)
    return grammar_checked_sentences


def generate_response_body(query_string):
    '''
    Generates a response body that includes proofreading data related to words and sentences.
    Returns this data as json.
    '''
    parser = HtmlParser()
    # parser.reset()
    if query_string is None or type(query_string) is not str:
        query_string = str("Incorrect query_string in generate_response_body()")
    response_body = {}
    try:
        parser.feed(query_string)
    except AssertionError:
        response_body['words'] = []
        response_body['htmldata'] = 'AssertionError'
    else:
        response_body['words'] = parser.words
        response_body['htmldata'] = parser.html
    # response_body['words'], response_body['htmldata'] = parser.feed(query_string)
    # parser.reset()

    # response_body['sentences'] = grammar_check_sentences(query_string)
    return json.dumps(response_body, indent=4)


@app.route('/')
def hello():
    '''
    Route to main page
    '''
    return 'This is a Finnish proofreading service that uses Voikko-library.\n'


@app.route('/api/v1/proofread')
def proofread():
    '''
    Route to proofread
    '''
    arg_text = request.args.get('text')
    if arg_text is None or type(arg_text) is not str:
        arg_text = 'no proofread string'
    response_body = generate_response_body(arg_text)
    headers = {"Content-Type": "application/json"}
    return make_response(response_body, 200, headers)


@app.route('/api/v1/proofread/add_word', methods=['POST'])
def add_word(word):
    '''
    Route for adding a word to dictionary
    '''
    # TODO: Check if word is valid
    # TODO: Check if word excist in a dictionary allready
    # TODO: Save word
    # TODO: Return a response that saving was successful


def init_app():
    return app
