import datetime
from datetime import timezone

import dateutil.parser

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.tim_models import AskedQuestion, AskedJson, db, LectureAnswer


class LectureTest(TimRouteTest):

    def test_lecture(self):
        self.login_test1()
        doc = self.create_doc(initial_par='testing lecture')
        name = doc.path
        current_time = datetime.datetime.now(tz=timezone.utc)
        time_format = '%H:%M:%S'
        start_time = (current_time - datetime.timedelta(minutes=15))
        end_time = (current_time + datetime.timedelta(hours=2))
        lecture_code = 'test lecture'
        j = self.post('/createLecture', query_string=dict(doc_id=doc.id,
                                                          end_date=end_time,
                                                          lecture_code=lecture_code,
                                                          max_students=50,
                                                          password='1234',
                                                          start_date=start_time))
        lecture_id = j['lectureId']
        self.assertIsInstance(lecture_id, int)
        j = self.get('/checkLecture', query_string=dict(doc_id=doc.id))

        self.assertDictEqual({'doc_name': name,
                              'endTime': end_time.isoformat(),
                              'isInLecture': True,
                              'isLecturer': True,
                              'lectureCode': lecture_code,
                              'lectureId': lecture_id,
                              'lecturers': [],  # TODO This is probably wrong, should have one element
                              'startTime': start_time.isoformat(),
                              'students': [],
                              'useQuestions': True,
                              'options': '{}',
                              'useWall': True}, j)

        resp = self.get('/getUpdates',
                        query_string=dict(c=-1,
                                          d=doc.id,
                                          m=True,
                                          l=lecture_id))
        self.assertEqual(-1, resp['e'])
        self.assertIsInstance(resp['ms'], int)

        msg_text = 'hi'
        j = self.post('/sendMessage', query_string=dict(lecture_id=lecture_id, message=msg_text))

        msg_id = j['id']
        self.assertIsInstance(msg_id, int)
        msg_datetime = dateutil.parser.parse(j['time'])
        msg_time = msg_datetime.strftime(time_format)
        resp = self.get('/getUpdates',
                        query_string=dict(c=-1,
                                          d=doc.id,
                                          m=True,
                                          l=lecture_id))

        self.check_time(current_time, resp, time_format)
        self.assert_dict_subset(resp, {'data': [{'message': msg_text, 'sender': self.current_user_name(), 'time': msg_time}],
                              'e': True,
                              'lastid': msg_id,
                              'lectureEnding': 100,
                              'lectureId': lecture_id,
                              'lecturers': [
                                  {'active': resp['lecturers'][0]['active'],
                                   'name': self.current_user_name()}],
                              'status': 'results',
                              'students': []})
        self.assertIsInstance(resp['ms'], int)

        resp = self.get('/getUpdates',
                        query_string=dict(c=msg_id,
                                          d=doc.id,
                                          m=True,
                                          l=lecture_id))

        self.assertEqual(-1, resp['e'])
        self.assertIsInstance(resp['ms'], int)

        # add some dummy data to test lecture deletion
        aj = AskedJson(json='{}', hash='')
        db.session.add(aj)
        db.session.flush()
        aq = AskedQuestion(lecture_id=lecture_id,
                          doc_id=doc.id,
                          par_id='test',
                          asked_time=datetime.datetime.now(tz=timezone.utc),
                          points='1',
                          asked_json_id=aj.asked_json_id,
                          expl='testing')
        db.session.add(aq)
        la = LectureAnswer(user_id=self.current_user.id,
                           question_id=aq.asked_id,
                           lecture_id=lecture_id,
                           answer='test',
                           answered_on=datetime.datetime.now(tz=timezone.utc),
                           points=1)
        db.session.add(la)
        db.session.commit()
        self.post('/deleteLecture', query_string={'lecture_id': lecture_id})
        self.post('/deleteLecture', query_string={'lecture_id': lecture_id}, expect_status=404)

    def check_time(self, current_time, resp, time_format):
        returned_time = datetime.datetime.strptime(resp['lecturers'][0]['active'], time_format).replace(
            tzinfo=timezone.utc)
        self.assertLess(returned_time - current_time, datetime.timedelta(seconds=2))
