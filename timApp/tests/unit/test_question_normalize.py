import unittest

from timApp.lecture.askedjson import normalize_question_json


class QuestionJsonNormalizeTest(unittest.TestCase):
    def test_normalize(self):
        result = normalize_question_json({
            "QUESTION": "8) Mitä muuttujia/viitteitä näkyy Luo-funktiossa",
            "TITLE": "8-Mitä näkyy Luo",
            "TYPE": "checkbox-vertical",
            "ANSWERFIELDTYPE": "checkbox",
            "MATRIXTYPE": "",
            "TIMELIMIT": 60,
            "DATA": {
                "HEADERS": [],
                "ROWS": [{
                    "id": 0,
                    "type": "question",
                    "text": "pallo",
                    "COLUMNS": [{
                        "id": 0,
                        "type": "answer",
                        "rowId": 0
                    }]
                }, {
                    "id": 1,
                    "type": "question",
                    "text": "x",
                    "COLUMNS": [{
                        "id": 0,
                        "type": "answer",
                        "rowId": 1
                    }]
                }, {
                    "id": 2,
                    "type": "question",
                    "text": "y",
                    "COLUMNS": [{
                        "id": 0,
                        "type": "answer",
                        "rowId": 2
                    }]
                }]
            }
        }
        )
        self.assertEqual({
            "questionText": "8) Mitä muuttujia/viitteitä näkyy Luo-funktiossa",
            "questionTitle": "8-Mitä näkyy Luo",
            "questionType": "checkbox-vertical",
            "answerFieldType": "checkbox",
            "timeLimit": 60,
            "headers": [],
            "rows": [{
                "id": 0,
                "type": "question",
                "text": "pallo",
                "columns": [{
                    "id": 0,
                    "type": "answer",
                    "rowId": 0
                }]
            }, {
                "id": 1,
                "type": "question",
                "text": "x",
                "columns": [{
                    "id": 0,
                    "type": "answer",
                    "rowId": 1
                }]
            }, {
                "id": 2,
                "type": "question",
                "text": "y",
                "columns": [{
                    "id": 0,
                    "type": "answer",
                    "rowId": 2
                }]
            }]
        }
            , result)
        result2 = normalize_question_json({
            "points": "4:1",
            "json": {
                "timeLimit": 50,
                "questionText": "11) Muutetaan rivi 33 muotoon Luo(this, ...).  Eli otetaan sijoitus pois.<br>Mit\u00e4 tapahtuu.",
                "questionType": "checkbox-vertical",
                "questionTitle": "muutetaan",
                "rows": ["Tulee yksi pallo (keltainen)", {
                    "text": "Tulee kaksi keltaista palloa",
                    "columns": [{
                        "rowId": 1,
                        "id": 0,
                        "type": "answer"
                    }],
                    "id": 1,
                    "type": "question"
                }, {"text": "N\u00e4kyy 1 valkoinen ja 1 punainen pallo",
                    "columns": [{
                        "rowId": 2,
                        "id": 0,
                        "type": "answer"
                    }],
                    "id": 2,
                    "type": "question"
                    }, ],
                "headers": [],
                "matrixType": "",
                "answerFieldType": "checkbox"
            },
            # "taskId": "muutetaan",
        }
        )
        self.assertEqual({
            "points": "4:1",
            "timeLimit": 50,
            "questionText": "11) Muutetaan rivi 33 muotoon Luo(this, ...).  Eli otetaan sijoitus pois.<br>Mit\u00e4 tapahtuu.",
            "questionType": "checkbox-vertical",
            "questionTitle": "muutetaan",
            "rows": ["Tulee yksi pallo (keltainen)", {
                "text": "Tulee kaksi keltaista palloa",
                "columns": [{
                    "rowId": 1,
                    "id": 0,
                    "type": "answer"
                }],
                "id": 1,
                "type": "question"
            }, {"text": "N\u00e4kyy 1 valkoinen ja 1 punainen pallo",
                "columns": [{
                    "rowId": 2,
                    "id": 0,
                    "type": "answer"
                }],
                "id": 2,
                "type": "question"
                }, ],
            "headers": [],
            "answerFieldType": "checkbox",
            # "taskId": "muutetaan",
        }, result2)

        result3 = normalize_question_json({
            "points": "",
            "json": {
                "data": {
                    "rows": [{
                        "text": "3 3 3",
                        "columns": [{
                            "rowId": 0,
                            "id": 0,
                            "type": "answer"
                        }],
                        "id": 0,
                        "type": "question"
                    }, {
                        "text": "3 4 5",
                        "columns": [{
                            "rowId": 1,
                            "id": 0,
                            "type": "answer"
                        }],
                        "id": 1,
                        "type": "question"
                    }],
                    "headers": []
                },
                "timeLimit": 40,
                "questionText": "Mit\u00e4 tulostaa",
                "questionType": "radio-vertical",
                "matrixType": "",
                "answerFieldType": "radio",
                "questionTitle": "Mit\u00e4 tulostaa"
            },
            "expl": {},
            # "taskId": ""
        }
        )
        self.assertEqual({
            "points": "",
            "rows": [{
                "text": "3 3 3",
                "columns": [{
                    "rowId": 0,
                    "id": 0,
                    "type": "answer"
                }],
                "id": 0,
                "type": "question"
            }, {
                "text": "3 4 5",
                "columns": [{
                    "rowId": 1,
                    "id": 0,
                    "type": "answer"
                }],
                "id": 1,
                "type": "question"
            }],
            "headers": [],
            "timeLimit": 40,
            "questionText": "Mit\u00e4 tulostaa",
            "questionType": "radio-vertical",
            "answerFieldType": "radio",
            "questionTitle": "Mit\u00e4 tulostaa",
            "expl": {},
            # "taskId": ""
        }, result3)
        result4 = normalize_question_json({
            "questionText": "Mit\u00e4 mielt\u00e4 olet t\u00e4st\u00e4 kysymyksest\u00e4?",
            "title": "kysymys",
            "timeLimit": 39601,
            "matrixType": "",
            "data": {
                "headers": [],
                "rows": [{
                    "text": "V\u00e4h\u00e4n ouroboros.",
                    "columns": [{
                        "id": 0,
                        "rowId": 0,
                        "type": "answer"
                    }],
                    "id": 0,
                    "type": "question"
                }, {
                    "text": "Ihan jees :)",
                    "columns": [{
                        "id": 0,
                        "rowId": 1,
                        "type": "answer"
                    }],
                    "id": 1,
                    "type": "question"
                }]
            },
            "questionType": "radio-vertical",
            "answerFieldType": "radio"
        }
        )
        self.assertEqual({
            "questionText": "Mit\u00e4 mielt\u00e4 olet t\u00e4st\u00e4 kysymyksest\u00e4?",
            "questionTitle": "kysymys",
            "timeLimit": 39601,
            "headers": [],
            "rows": [{
                "text": "V\u00e4h\u00e4n ouroboros.",
                "columns": [{
                    "id": 0,
                    "rowId": 0,
                    "type": "answer"
                }],
                "id": 0,
                "type": "question"
            }, {
                "text": "Ihan jees :)",
                "columns": [{
                    "id": 0,
                    "rowId": 1,
                    "type": "answer"
                }],
                "id": 1,
                "type": "question"
            }],
            "questionType": "radio-vertical",
            "answerFieldType": "radio"
        }
            , result4)
        self.assertEqual(result4, normalize_question_json(result4))
        result4.update(isTask=True)
        self.assertEqual(result4, normalize_question_json(result4, allow_top_level_keys={'isTask'}))

    def test_invalid(self):
        self.assertEqual({'answerFieldType': 'text',
                          'expl': {},
                          'headers': [''],
                          'invalid': True,
                          'isTask': True,
                          'matrixType': 'textArea',
                          'questionText': 'Invalid question data: Missing field: questionTitle',
                          'questionTitle': 'Invalid question data: Missing field: questionTitle',
                          'questionType': 'matrix',
                          'rows': ['']}, normalize_question_json({}))
        self.assertEqual({'answerFieldType': 'text',
                          'expl': {},
                          'headers': [''],
                          'invalid': True,
                          'isTask': True,
                          'matrixType': 'textArea',
                          'questionText': 'Invalid question data: Missing fields: answerFieldType, '
                                          'headers, questionText, questionType, rows',
                          'questionTitle': 'Invalid question data: Missing fields: answerFieldType, '
                                           'headers, questionText, questionType, rows',
                          'questionType': 'matrix',
                          'rows': ['']}, normalize_question_json({'questionTitle': ''}))
        self.assertEqual({'answerFieldType': 'text',
                          'expl': {},
                          'headers': [''],
                          'invalid': True,
                          'isTask': True,
                          'matrixType': 'textArea',
                          'questionText': 'Invalid question data: Missing matrixType when questionType '
                                          'is matrix',
                          'questionTitle': 'Invalid question data: Missing matrixType when questionType '
                                           'is matrix',
                          'questionType': 'matrix',
                          'rows': ['']}, normalize_question_json({
            "questionText": "test",
            "questionTitle": "test",
            "timeLimit": 1,
            "headers": [],
            "rows": [""],
            "questionType": "matrix",
            "answerFieldType": "radio"
        }))
        self.assertEqual({'answerFieldType': 'text',
                          'expl': {},
                          'headers': [''],
                          'invalid': True,
                          'isTask': True,
                          'matrixType': 'textArea',
                          'questionText': 'Invalid question data: A row must be a dictionary or a '
                                          'string',
                          'questionTitle': 'Invalid question data: A row must be a dictionary or a '
                                           'string',
                          'questionType': 'matrix',
                          'rows': ['']}, normalize_question_json({
            "questionText": "test",
            "questionTitle": "test",
            "timeLimit": 1,
            "headers": [],
            "rows": [0],
            "questionType": "radiobutton-horizontal",
            "answerFieldType": "radio"
        }))
