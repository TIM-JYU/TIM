import requests
from requests.adapters import HTTPAdapter
from flask import Response
import json


# Returns proofread-object
def proofread_text(text):
    s = requests.Session()
    s.mount('http://oiko:5000/api/v1/proofread', HTTPAdapter(max_retries=5))

    url = "http://oiko:5000/api/v1/proofread"
    ntext = ''
    if type(text) is str:
        ntext = text

    try:
        response = requests.get(url, params={'text': ntext})
    except requests.exceptions.ConnectionError:
        return {'words': [], 'htmldata': text}

    return json.loads(response.text)


# Returns proofread-object
def proofread_pars(pars):

    '''Tässä tehdään kutsu oiko-konttiin. Tuo request-funktio palauttaa flaskin Response-objektin.
    Tässä vaiheessa oikolukukonttiin lähetetään siis vain tuo kovakoodattu tekstinpätkä,
    tarkoitus olisi, että kyseinen haku tehtäisiin pars-muuttujan sisältämistä teksteistä.
    Tuo pars sisältää taulukon, jossa on objekteja, jotka sisältävät mm. paragraphien tekstisisällön,
    markdownin ja html:n kissa `istuu` <- koodin merkintä'''

    s = requests.Session()
    s.mount('http://oiko:5000/api/v1/proofread', HTTPAdapter(max_retries=5))

    # Url-enkoodaus
    url = "http://oiko:5000/api/v1/proofread"
    text = ''
    if len(pars) != 0:
        text = pars[0]['html']
    try:
        response = requests.get(url, params={'text': text})
    except requests.exceptions.ConnectionError:
        return {'words': [], 'htmldata': text}

    # Tässä palautetaan Response-objektin tekstimuotoinen versio jsoniksi muunnettuna
    return json.loads(response.text)


