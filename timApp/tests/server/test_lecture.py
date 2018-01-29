import datetime
from datetime import timezone

import dateutil.parser

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.tim_models import db
from timApp.timdb.models.lectureanswer import LectureAnswer
from timApp.timdb.models.askedjson import AskedJson
from timApp.timdb.models.askedquestion import AskedQuestion


class LectureTest(TimRouteTest):

    def test_lecture(self):
        self.login_test1()
        doc = self.create_doc(initial_par='testing lecture')
        current_time = datetime.datetime.now(tz=timezone.utc)
        time_format = '%H:%M:%S'
        start_time = (current_time - datetime.timedelta(minutes=15))
        end_time = (current_time + datetime.timedelta(hours=2))
        lecture_code = 'test lecture'
        j = self.json_post('/createLecture', json_data=dict(doc_id=doc.id,
                                                            end_time=end_time,
                                                            lecture_code=lecture_code,
                                                            max_students=50,
                                                            password='1234',
                                                            start_time=start_time))
        lecture_id = j['lecture_id']
        self.assertIsInstance(lecture_id, int)
        j = self.get('/checkLecture', query_string=dict(doc_id=doc.id))

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
                                  'options': '{}',
                                  'doc_id': doc.id,
                                  'lecturer': self.current_user.id,
                                  'is_access_code': True,
                                  'password': None,
                                  'is_full': False
                              }}, j)

        resp = self.get('/getUpdates',
                        query_string=dict(c=-1,
                                          d=doc.id,
                                          m=True))
        self.assertIsInstance(resp['ms'], int)

        msg_text = 'hi'
        j = self.post('/sendMessage', query_string=dict(message=msg_text))

        msg_id = j['msg_id']
        self.assertIsInstance(msg_id, int)
        msg_datetime = dateutil.parser.parse(j['timestamp'])
        resp = self.get('/getUpdates',
                        query_string=dict(c=-1,
                                          d=doc.id,
                                          m=True))

        self.check_time(current_time, resp, time_format)
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

        resp = self.get('/getUpdates',
                        query_string=dict(c=msg_id,
                                          d=doc.id,
                                          m=True))
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
        self.post('/deleteLecture', query_string={'lecture_id': lecture_id})
        self.post('/deleteLecture', query_string={'lecture_id': lecture_id}, expect_status=404)

    def check_time(self, current_time, resp, time_format):
        returned_time = datetime.datetime.strptime(resp['lecturers'][0]['active'], time_format).replace(
            tzinfo=timezone.utc)
        self.assertLess(returned_time - current_time, datetime.timedelta(seconds=2))
