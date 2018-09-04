import datetime
import json
from datetime import timezone
from time import sleep

import dateutil.parser

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.lecture.askedjson import AskedJson, make_error_question
from timApp.lecture.askedquestion import AskedQuestion, get_asked_question
from timApp.lecture.lecture import Lecture
from timApp.lecture.lectureanswer import LectureAnswer
from timApp.lecture.showpoints import Showpoints
from timApp.timdb.sqa import db
from timApp.util.utils import EXAMPLE_DOCS_PATH


class LectureTest(TimRouteTest):

    def get_updates(self, doc_id: int, msg_id: int, use_questions: bool = None, curr_q: int = None, curr_p: int = None,
                    **kwargs):
        return self.get('/getUpdates', query_string=dict(c=msg_id,
                                                         d=doc_id,
                                                         m=True,
                                                         q=use_questions,
                                                         i=curr_q,
                                                         p=curr_p,
                                                         ), **kwargs)

    def test_lecture(self):
        self.login_test1()
        doc = self.create_doc(from_file=f'{EXAMPLE_DOCS_PATH}/questions.md')
        current_time = datetime.datetime.now(tz=timezone.utc)
        start_time = (current_time - datetime.timedelta(minutes=15))
        end_time = (current_time + datetime.timedelta(hours=2))
        lecture_code = 'test lecture'
        self.get('/checkLecture', query_string=dict(doc_id=doc.id))
        self.json_post('/startFutureLecture', query_string=dict(lecture_code='xxx', doc_id=doc.id), expect_status=404,
                       expect_content={"error": "Lecture not found"})
        j = self.json_post('/createLecture', json_data=dict(doc_id=doc.id,
                                                            end_time=end_time,
                                                            lecture_code=lecture_code,
                                                            max_students=50,
                                                            password='1234',
                                                            start_time=start_time))
        lecture_id = j['lecture_id']
        lecture_q = {'lecture_id': lecture_id}
        self.assertIsInstance(lecture_id, int)
        j = self.get('/checkLecture', query_string=dict(doc_id=doc.id))

        # TODO: Check also other than status code
        self.get(f'/getAllLecturesFromDocument', query_string=dict(doc_id=doc.id))
        self.get(f'/getAllMessages', query_string=lecture_q)
        self.get(f'/getLectureAnswerTotals/{lecture_id}')
        self.get(f'/getLectureByCode', query_string=dict(doc_id=doc.id, lecture_code=lecture_code))
        self.get(f'/getLectureInfo', query_string=lecture_q)
        self.get(f'/showLectureInfo/{lecture_id}')
        self.get(f'/showLectureInfoGivenName', query_string=lecture_q)

        self.assertDictEqual({'isInLecture': True,
                              'isLecturer': True,
                              'lecturers': [],  # TODO This is probably wrong, should have one element
                              'students': [],
                              'useQuestions': True,
                              'useWall': True,
                              'lecture': {
                                  'lecture_code': lecture_code,
                                  'lecture_id': lecture_id,
                                  'end_time': end_time.isoformat(),
                                  'start_time': start_time.isoformat(),
                                  'options': {},
                                  'doc_id': doc.id,
                                  'lecturer': self.current_user.id,
                                  'is_access_code': True,
                                  'password': None,
                                  'is_full': False
                              }}, j)
        resp = self.get_updates(doc.id, -1)
        self.assertIsInstance(resp['ms'], int)

        msg_text = 'hi'
        j = self.post('/sendMessage', query_string=dict(message=msg_text))

        msg_id = j['msg_id']
        self.assertIsInstance(msg_id, int)
        msg_datetime = dateutil.parser.parse(j['timestamp'])
        resp = self.get_updates(doc.id, -1)

        self.check_time(current_time, resp)
        u = {'email': 'test1@example.com', 'id': 4, 'name': 'testuser1', 'real_name': 'Test user 1'}
        self.assert_dict_subset(resp, {
            'msgs': [{
                'message': msg_text,
                'msg_id': msg_id,
                'user': u, 'timestamp': msg_datetime.isoformat()}],
            'lectureEnding': 100,
            'lectureId': lecture_id,
            'lecturers': [
                {'active': resp['lecturers'][0]['active'],
                 'user': u, }],
            'students': [],
        })
        self.assertIsInstance(resp['ms'], int)

        resp = self.get_updates(doc.id, msg_id)
        self.assertIsInstance(resp['ms'], int)
        self.assertEqual(resp['msgs'], [])

        # add some dummy data to test lecture deletion
        aj = AskedJson(json='{}', hash='')
        aq = AskedQuestion(lecture_id=lecture_id,
                           doc_id=doc.id,
                           par_id='test',
                           asked_time=datetime.datetime.now(tz=timezone.utc),
                           points='1',
                           expl='testing',
                           asked_json=aj)
        la = LectureAnswer(user_id=self.current_user.id,
                           lecture_id=lecture_id,
                           answer='test',
                           answered_on=datetime.datetime.now(tz=timezone.utc),
                           points=1,
                           asked_question=aq)
        db.session.add(la)
        db.session.commit()

        par_id = doc.document.get_paragraphs()[0].get_id()
        resp = self.json_post('/askQuestion',
                              query_string=dict(doc_id=doc.id, par_id=par_id))
        aid = resp['asked_id']
        q = get_asked_question(aid)
        self.assertIsNotNone(q.running_question)
        resp = self.get_updates(
            doc.id,
            msg_id,
            True,
            expect_contains={'extra':
                                 {'data':
                                      {'asked_id': aid,
                                       'asked_time': resp['asked_time'],
                                       'doc_id': doc.id,
                                       'json': {
                                           'hash': 'MHgzN2M5NGNhYQ==',
                                           'json': {
                                               'answerFieldType': 'radio',
                                               'headers': [],
                                               'points': '2:1',
                                               'questionText': 'What day is it today?',
                                               'questionTitle': 'Today',
                                               'questionType': 'radio-vertical',
                                               'rows': [
                                                   'Monday',
                                                   'Wednesday',
                                                   'Friday'],
                                               'timeLimit': 90}},
                                       'lecture_id': lecture_id,
                                       'par_id': par_id},
                                  'type': 'question'}})
        resp = self.get_updates(doc.id, msg_id, True, aid)
        self.assertIsNone(resp.get('extra'))

        self.json_put('/answerToQuestion', query_string={'input': json.dumps({'answers': [['0']]}), 'asked_id': aid},
                      expect_content=self.ok_resp)
        self.json_put('/answerToQuestion', query_string={'input': json.dumps({'answers': [['0']]}), 'asked_id': aid},
                      expect_content={'alreadyAnswered': 'You have already answered to question. Your first answer '
                                                         'is saved.'})

        self.login_test2()
        resp = self.json_post('/joinLecture', query_string={'password_quess': '1234', **lecture_q})
        self.assertTrue(resp['correctPassword'])
        resp = self.get_updates(doc.id, msg_id, True)
        self.assertEqual(resp['extra']['type'], 'question')

        self.login_test1()
        self.post('/extendQuestion', query_string=dict(asked_id=aid, extend=1))

        self.login_test2()
        resp = self.get_updates(doc.id, msg_id, True, aid)
        dateutil.parser.parse(resp['extra']['new_end_time'])  # ensure valid timestamp

        self.json_put('/answerToQuestion', query_string={'input': json.dumps({'answers': [['1']]}), 'asked_id': aid},
                      expect_content=self.ok_resp)

        resp = self.get_updates(doc.id, msg_id, True, aid)
        self.assertIsNotNone(resp.get('extra'))
        resp = self.get_updates(doc.id, msg_id, True, aid)
        self.assertIsNotNone(resp.get('extra'))

        self.login_test1()
        self.post('/stopQuestion', query_string=dict(asked_id=aid))

        self.login_test2()
        resp = self.get_updates(doc.id, msg_id, True, aid)
        self.assertEqual(resp.get('extra'), {'new_end_time': None})

        sp = Showpoints.query.get(aid)
        self.assertIsNone(sp)

        self.login_test1()

        self.post('/showAnswerPoints', query_string=dict(asked_id=aid))
        db.session.remove()
        sp = Showpoints.query.get(aid)
        self.assertIsNotNone(sp)

        resp = self.get_updates(doc.id, msg_id, True, aid)
        self.assertEqual(resp.get('extra'), {'new_end_time': None})
        resp = self.get_updates(doc.id, msg_id, True, aid)
        self.assertEqual(resp.get('extra'), {'new_end_time': None})

        q = get_asked_question(aid)
        self.assertIsNone(q.running_question)

        self.login_test2()

        resp = self.get_updates(doc.id, msg_id, True)
        self.assertEqual(resp['extra']['type'], 'result')
        resp = self.get_updates(doc.id, msg_id, True, curr_p=aid)
        self.assertIsNone(resp.get('extra'))
        self.json_put('/closePoints', query_string=dict(asked_id=aid))
        resp = self.get_updates(doc.id, msg_id, True, curr_p=aid)
        self.assertEqual(resp.get('extra'), {'points_closed': True})

        par_id = doc.document.get_paragraphs()[1].get_id()

        self.login_test1()

        self.post('/updatePoints/', query_string=dict(asked_id=aid, points='2:1'))

        resp = self.json_post('/askQuestion',
                              query_string=dict(doc_id=doc.id, par_id=par_id))
        aid = resp['asked_id']
        q = get_asked_question(aid)
        self.assertIsNotNone(q.running_question)

        # should be stopped by the background thread
        sleep(2)
        q = get_asked_question(aid)
        db.session.refresh(q)
        self.assertIsNone(q.running_question)

        self.post('/extendLecture',
                  query_string={**lecture_q,
                                'new_end_time': (current_time + datetime.timedelta(hours=3))})

        self.post('/joinLecture', query_string=lecture_q)
        self.get('/getLectureAnswers', query_string=dict(asked_id=aid))
        self.post('/endLecture', query_string=lecture_q)
        self.post('/leaveLecture', query_string=lecture_q)
        self.post('/deleteLecture', query_string=lecture_q)
        self.post('/deleteLecture', query_string=lecture_q, expect_status=404)

    def check_time(self, current_time, resp):
        returned_time = dateutil.parser.parse(resp['lecturers'][0]['active'])
        self.assertLess(returned_time - current_time, datetime.timedelta(seconds=2))

    def test_invalid_max_students(self):
        self.login_test1()
        d = self.create_doc()
        l = Lecture(doc_id=d.id, lecturer=self.current_user_id(), options=json.dumps({'max_students': ''}))
        self.assertIsNone(l.max_students)

    def test_empty_lectureanswer(self):
        self.login_test1()
        d = self.create_doc()
        l = Lecture(doc_id=d.id,
                    lecturer=self.current_user_id(),
                    start_time=datetime.datetime.now())
        db.session.add(l)
        db.session.flush()
        aj = AskedJson(json='{}', hash='asd')
        q = AskedQuestion(lecture=l, asked_json=aj)
        a = LectureAnswer(lecture_id=l.lecture_id, asked_question=q, answer='')
        self.assertEqual(a.to_json()['answer'], [])

    def test_askedjson(self):
        j = make_error_question('')
        j['points'] = '2:3'
        aj = AskedJson(json=json.dumps(j))
        aq = AskedQuestion(asked_json=aj)
        self.assertEqual('2:3', aq.get_effective_points())
        aq.points = '2:4'
        self.assertEqual('2:4', aq.get_effective_points())
        del j['points']
        aj = AskedJson(json=json.dumps(j))
        aq = AskedQuestion(asked_json=aj)
        self.assertEqual(None, aq.get_effective_points())
