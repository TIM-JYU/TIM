from flask import Flask, request, jsonify
from voikko.libvoikko import Voikko, Token

app = Flask(__name__)


@app.route('/api/v1/proofread', methods=['post'])
def proofread():
    phrases = request.get_json()
    # The Voikko object is NOT thread-safe; we cannot initialize it in module level.
    voikko = Voikko("fi")
    tokenized = [voikko.tokens(p) for p in phrases]

    def get_spelling_info(s: str):
        is_correct = voikko.spell(s)
        if not is_correct:
            return [is_correct, voikko.suggest(s)]
        else:
            return [is_correct, []]

    spelling = {
        token.tokenText: get_spelling_info(token.tokenText) for t in tokenized for token in t if
        token.tokenType == Token.WORD
    }

    return jsonify({
        'spelling': spelling,
        'tokenlists': [[[x.tokenText, x.tokenType] for x in t] for t in tokenized],
    })
