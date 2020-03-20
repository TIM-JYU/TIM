import json
from collections import defaultdict
from dataclasses import dataclass, field
from typing import List, Tuple, Dict

import requests
from bs4 import BeautifulSoup, PageElement


@dataclass
class Word:
    word: str
    suggestions: List[str]


@dataclass
class SpellCheckResult:
    words: List[Word]
    new_html: str


def proofread_pars(pars: List[dict]) -> List[SpellCheckResult]:
    return [process_spelling_errors(p['html']) for p in pars]


banned_tags = {'code'}
banned_classes = {'math', 'mathp', 'nospell'}


def is_banned(e: PageElement) -> bool:
    return any(p.name in banned_tags or (set(p.get('class', [])) & banned_classes) for p in e.parents)


@dataclass
class VoikkoToken:
    tokenType: int
    tokenText: str


@dataclass
class VoikkoClient:
    """Provides an API to libvoikko that exists in another container as an HTTP service.
    The API is intended to be identical to libvoikko.
    """
    data: List[str]
    phrase_data: Dict[str, List[VoikkoToken]] = field(init=False)
    spelling: Dict[str, Tuple[bool, List[str]]] = field(init=False)

    def __post_init__(self):
        r = requests.post('http://oiko:5000/api/v1/proofread', json=self.data)
        results = r.json()
        self.spelling = results['spelling']
        self.phrase_data = dict(
            zip(
                self.data,
                [[VoikkoToken(tokenText=t[0], tokenType=t[1]) for t in tokenlist] for tokenlist in results['tokenlists']],
            )
        )

    def tokens(self, s: str) -> List[VoikkoToken]:
        return self.phrase_data[s]

    def spell(self, s: str) -> bool:
        return self.spelling[s][0]

    def suggest(self, s: str) -> List[str]:
        return self.spelling[s][1]


def process_spelling_errors(s: str) -> SpellCheckResult:
    bs = BeautifulSoup(s, 'lxml')
    words = []
    word_occurrence_counts = defaultdict(int)
    text_elements = [e for e in bs.find_all(text=True) if not is_banned(e)]
    voikko = VoikkoClient([str(e) for e in text_elements])

    for e in text_elements:  # type: PageElement
        parts = []
        has_errors = False
        for word in voikko.tokens(str(e)):
            # Token type 1 is word.
            if word.tokenType == 1 and not voikko.spell(word.tokenText):
                w = Word(word=word.tokenText, suggestions=voikko.suggest(word.tokenText))
                count = word_occurrence_counts[w.word]
                count += 1
                word_occurrence_counts[w.word] = count
                words.append(w)
                se = bs.new_tag('tim-spell-error', attrs={'bind-sugg': json.dumps(w.suggestions)})
                if count > 1:
                    se['bind-count'] = count
                se.string = w.word
                parts.append(se)
                has_errors = True
            else:
                parts.append(word.tokenText)
        if has_errors:
            n = BeautifulSoup("", 'lxml')
            for f in parts:
                n.append(f)
            e.replace_with(n)

    # Unwrap html and body tags.
    new_html = ''.join(str(e) for e in bs.contents[0].contents[0].contents) if bs.contents else ''

    return SpellCheckResult(words=words, new_html=new_html)
