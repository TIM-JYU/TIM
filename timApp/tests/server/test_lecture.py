import datetime
import json
from time import sleep

import dateutil.parser

from timApp.lecture.askedjson import AskedJson, make_error_question
from timApp.lecture.askedquestion import AskedQuestion, get_asked_question
from timApp.lecture.lecture import Lecture
from timApp.lecture.lectureanswer import LectureAnswer
from timApp.lecture.showpoints import Showpoints
from timApp.tests.db.timdbtest import TEST_USER_1_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time, static_tim_doc


class LectureTest(TimRouteTest):
    def get_updates(
        self,
        doc_id: int,
        msg_id: int,
        use_questions: bool | None = None,
        curr_q: int | None = None,
        curr_p: int | None = None,
        **kwargs,
    ):
        return self.get(
            "/getUpdates",
            query_string=dict(
                c=msg_id,
                d=doc_id,
                m=True,
                q=use_questions,
                i=curr_q,
                p=curr_p,
            ),
            **kwargs,
        )

    def test_lecture(self):
        self.login_test1()
        doc = self.create_doc(from_file=static_tim_doc("questions.md"))
        current_time = get_current_time()
        start_time = current_time - datetime.timedelta(minutes=15)
        end_time = current_time + datetime.timedelta(hours=2)
        lecture_code = "test lecture"
        self.get("/checkLecture", query_string=dict(doc_id=doc.id))
        self.json_post(
            "/startFutureLecture",
            query_string=dict(lecture_code="xxx", doc_id=doc.id),
            expect_status=404,
            expect_content={"error": "Lecture not found"},
        )
        j = self.json_post(
            "/createLecture",
            json_data=dict(
                doc_id=doc.id,
                end_time=end_time,
                lecture_code=lecture_code,
                max_students=50,
                password="1234",
                start_time=start_time,
            ),
        )
        lecture_id = j["lecture_id"]
        lecture_q = {"lecture_id": lecture_id}
        self.assertIsInstance(lecture_id, int)
        j = self.get("/checkLecture", query_string=dict(doc_id=doc.id))

        # TODO: Check also other than status code
        self.get(f"/getAllLecturesFromDocument", query_string=dict(doc_id=doc.id))
        self.get(f"/getAllMessages", query_string=lecture_q)
        self.get(
            f"/getLectureByCode",
            query_string=dict(doc_id=doc.id, lecture_code=lecture_code),
        )
        self.get(f"/getLectureInfo", query_string=lecture_q)
        self.get(f"/showLectureInfo/{lecture_id}")
        self.get(f"/showLectureInfoGivenName", query_string=lecture_q)

        self.assertDictEqual(
            {
                "isInLecture": True,
                "isLecturer": True,
                "lecturers": [],  # TODO This is probably wrong, should have one element
                "students": [],
                "useQuestions": True,
                "lecture": {
                    "lecture_code": lecture_code,
                    "lecture_id": lecture_id,
                    "end_time": end_time.isoformat(),
                    "start_time": start_time.isoformat(),
                    "options": {},
                    "doc_id": doc.id,
                    "lecturer": self.current_user.id,
                    "is_access_code": True,
                    "password": None,
                    "is_full": False,
                },
            },
            j,
        )
        resp = self.get_updates(doc.id, -1)
        self.assertIsInstance(resp["ms"], int)

        msg_text = "hi"
        j = self.post("/sendMessage", query_string=dict(message=msg_text))

        msg_id = j["msg_id"]
        self.assertIsInstance(msg_id, int)
        msg_datetime = dateutil.parser.parse(j["timestamp"])
        resp = self.get_updates(doc.id, -1)

        u = {
            "email": "test1@example.com",
            "id": TEST_USER_1_ID,
            "name": "testuser1",
            "real_name": "Test user 1",
        }
        self.assert_dict_subset(
            resp,
            {
                "msgs": [
                    {
                        "message": msg_text,
                        "msg_id": msg_id,
                        "user": u,
                        "timestamp": msg_datetime.isoformat(),
                    }
                ],
                "lectureEnding": 100,
                "lectureId": lecture_id,
                "lecturers": [
                    {
                        "user": u,
                        "activeSecondsAgo": resp["lecturers"][0]["activeSecondsAgo"],
                    }
                ],
                "students": [],
            },
        )
        self.assertIsInstance(resp["ms"], int)

        resp = self.get_updates(doc.id, msg_id)
        self.assertIsInstance(resp["ms"], int)
        self.assertEqual(resp["msgs"], [])

        # add some dummy data to test lecture deletion
        aj = AskedJson(json="{}", hash="")
        aq = AskedQuestion(
            lecture_id=lecture_id,
            doc_id=doc.id,
            par_id="test",
            asked_time=get_current_time(),
            points="1",
            expl="{}",
            asked_json=aj,
        )
        la = LectureAnswer(
            user_id=self.current_user.id,
            lecture_id=lecture_id,
            answer="test",
            answered_on=get_current_time(),
            points=1,
            asked_question=aq,
        )
        db.session.add(la)
        db.session.commit()

        par_id = doc.document.get_paragraphs()[0].get_id()
        resp = self.json_post(
            "/askQuestion", query_string=dict(doc_id=doc.id, par_id=par_id)
        )
        aid = resp["asked_id"]
        q = get_asked_question(aid)
        self.assertIsNotNone(q.running_question)
        resp = self.get_updates(
            doc.id,
            msg_id,
            True,
            expect_contains={
                "extra": {
                    "data": {
                        "asked_id": aid,
                        "asked_time": resp["asked_time"],
                        "doc_id": doc.id,
                        "json": {
                            "hash": "LTB4MzJjNWJkYTM=",
                            "json": {
                                "answerFieldType": "radio",
                                "defaultPoints": 0.5,
                                "headers": [],
                                "points": "2:1",
                                "questionText": "What day is it today?",
                                "questionTitle": "Today",
                                "questionType": "radio-vertical",
                                "rows": ["Monday", "Wednesday", "Friday"],
                                "timeLimit": 90,
                            },
                        },
                        "lecture_id": lecture_id,
                        "par_id": par_id,
                    },
                    "type": "question",
                }
            },
        )
        resp = self.get_updates(doc.id, msg_id, True, aid)
        self.assertIsNone(resp.get("extra"))

        self.json_put(
            "/answerToQuestion",
            json_data={"input": [["0"]], "asked_id": aid},
            expect_content=self.ok_resp,
        )
        self.json_put(
            "/answerToQuestion",
            json_data={"input": [["0"]], "asked_id": aid},
            expect_content={
                "alreadyAnswered": "You have already answered to question. Your first answer "
                "is saved."
            },
        )

        self.login_test2()
        resp = self.json_post(
            "/joinLecture", query_string={"password_quess": "1234", **lecture_q}
        )
        self.assertTrue(resp["correctPassword"])
        resp = self.get_updates(doc.id, msg_id, True)
        self.assertEqual(resp["extra"]["type"], "question")

        self.login_test1()
        self.post("/extendQuestion", query_string=dict(asked_id=aid, extend=1))

        self.login_test2()
        resp = self.get_updates(doc.id, msg_id, True, aid)
        original_end_time = dateutil.parser.parse(resp["question_end_time"])

        self.json_put(
            "/answerToQuestion",
            json_data={"input": [["2"]], "asked_id": aid},
            expect_content=self.ok_resp,
        )

        resp = self.get_updates(doc.id, msg_id, True, aid)
        # self.assertIsNotNone(resp.get('extra'))
        resp = self.get_updates(doc.id, msg_id, True, aid)
        # self.assertIsNotNone(resp.get('extra'))

        self.login_test1()
        self.post(
            "/stopQuestion",
            query_string=dict(asked_id=aid),
            expect_content={"status": "ok"},
        )
        self.post(
            "/stopQuestion",
            query_string=dict(asked_id=aid),
            expect_content={"status": "Question not running anymore"},
        )

        self.login_test2()
        resp = self.get_updates(doc.id, msg_id, True, aid)
        new_end_time = dateutil.parser.parse(resp["question_end_time"])
        self.assertTrue(original_end_time > new_end_time)

        sp = db.session.get(Showpoints, aid)
        self.assertIsNone(sp)

        self.login_test1()

        self.post("/showAnswerPoints", query_string=dict(asked_id=aid))
        db.session.remove()
        sp = db.session.get(Showpoints, aid)
        self.assertIsNotNone(sp)

        resp = self.get_updates(doc.id, msg_id, True, aid)
        self.assertEqual(resp["extra"]["type"], "result")
        resp = self.get_updates(doc.id, msg_id, True, curr_p=aid)
        self.assertEqual(resp.get("extra"), None)

        q = get_asked_question(aid)
        self.assertFalse(q.is_running)

        self.login_test2()

        resp = self.get_updates(doc.id, msg_id, True)
        self.assertEqual(resp["extra"]["type"], "result")
        resp = self.get_updates(doc.id, msg_id, True, curr_p=aid)
        self.assertIsNone(resp.get("extra"))
        self.json_put("/closePoints", query_string=dict(asked_id=aid))
        resp = self.get_updates(doc.id, msg_id, True, curr_p=aid)
        self.assertEqual(resp.get("extra"), {"points_closed": True})

        par_id = doc.document.get_paragraphs()[1].get_id()

        self.login_test1()

        totals = self.get(f"/getLectureAnswerTotals/{lecture_id}")
        self.assertEqual(
            """
testuser1;sum;1.5
testuser2;sum;1.0

testuser1;count;2
testuser2;count;1
                """.strip()
            + "\n",
            totals,
        )
        # updatePoints should take defaultPoints 0.5 into account
        self.post("/updatePoints/", query_string=dict(asked_id=aid, points="0:1"))

        resp = self.json_post(
            "/askQuestion", query_string=dict(doc_id=doc.id, par_id=par_id)
        )
        aid2 = resp["asked_id"]
        q = get_asked_question(aid2)
        self.assertIsNotNone(q.running_question)

        sleep(1)  # the second question has a timelimit of 1 second
        q = get_asked_question(aid2)
        db.session.refresh(q)
        self.assertFalse(q.is_running)
        l: Lecture = db.session.get(Lecture, lecture_id)
        self.assertEqual(1, len(l.running_questions))

        self.json_post("/extendLecture", {}, expect_status=400)
        self.post(
            "/extendLecture",
            query_string={
                **lecture_q,
                "new_end_time": (current_time + datetime.timedelta(hours=3)),
            },
        )

        self.post("/joinLecture", query_string=lecture_q)
        r = self.get("/getLectureAnswers", query_string=dict(asked_id=aid2))
        self.assertEqual([], r)
        r = self.get("/getLectureAnswers", query_string=dict(asked_id=aid))
        self.assertEqual(2, len(r))
        timestr = get_current_time().isoformat().replace("+00:00", "Z")
        self.assertTrue(timestr.endswith("Z"))
        r = self.get(
            "/getLectureAnswers", query_string=dict(asked_id=aid, after=timestr)
        )
        self.assertEqual(0, len(r))
        self.post("/endLecture", query_string=lecture_q)
        self.post("/leaveLecture", query_string=lecture_q)

        totals = self.get(f"/getLectureAnswerTotals/{lecture_id}")
        self.assertEqual(
            """
testuser1;sum;2.0
testuser2;sum;0.5

testuser1;count;2
testuser2;count;1
        """.strip()
            + "\n",
            totals,
        )

        r = self.get(f"/getLectureInfo", query_string=lecture_q)
        self.assertEqual(2, len(r["answerers"]))
        self.assertEqual(3, len(r["answers"]))
        self.login_test3()
        r = self.get(f"/getLectureInfo", query_string=lecture_q)
        self.assertEqual(1, len(r["answerers"]))
        self.assertEqual(self.current_user.name, r["answerers"][0]["name"])
        self.assertEqual(0, len(r["answers"]))
        self.assertEqual(3, len(r["questions"]))
        self.login_test1()

        self.post("/deleteLecture", query_string=lecture_q)
        self.post("/deleteLecture", query_string=lecture_q, expect_status=404)

    def test_invalid_max_students(self):
        self.login_test1()
        d = self.create_doc()
        l = Lecture(
            doc_id=d.id,
            lecturer=self.current_user_id(),
            options=json.dumps({"max_students": ""}),
        )
        self.assertIsNone(l.max_students)

    def test_empty_lectureanswer(self):
        self.login_test1()
        d = self.create_doc()
        l = Lecture(
            doc_id=d.id,
            lecturer=self.current_user_id(),
            start_time=datetime.datetime.now(),
        )
        db.session.add(l)
        db.session.flush()
        aj = AskedJson(json="{}", hash="asd")
        q = AskedQuestion(lecture=l, asked_json=aj)
        a = LectureAnswer(lecture_id=l.lecture_id, asked_question=q, answer="")
        self.assertEqual(a.to_json()["answer"], [])

    def test_askedjson(self):
        j = make_error_question("")
        j["points"] = "2:3"
        aj = AskedJson(json=json.dumps(j))
        aq = AskedQuestion(asked_json=aj)
        self.assertEqual("2:3", aq.get_effective_points())
        aq.points = "2:4"
        self.assertEqual("2:4", aq.get_effective_points())
        del j["points"]
        aj = AskedJson(json=json.dumps(j))
        aq = AskedQuestion(asked_json=aj)
        self.assertEqual(None, aq.get_effective_points())

    def test_no_multiple_lectures(self):
        """User won't join to multiple lectures if he creates many of them."""
        self.login_test1()
        d = self.create_doc()
        curr = get_current_time()
        end_time = curr + datetime.timedelta(minutes=5)
        self.json_post(
            "/createLecture",
            json_data=dict(
                doc_id=d.id,
                end_time=end_time,
                lecture_code="t1",
                max_students=50,
                password="1234",
                start_time=curr,
            ),
        )
        self.json_post(
            "/createLecture",
            json_data=dict(
                doc_id=d.id,
                end_time=end_time,
                lecture_code="t2",
                max_students=50,
                password="1234",
                start_time=curr,
            ),
        )
        self.get_updates(d.id, -1)

    def test_shuffled_questions(self):
        self.login_test1()
        current_time = get_current_time()
        start_time = current_time - datetime.timedelta(minutes=15)
        end_time = current_time + datetime.timedelta(hours=2)
        lecture_code = "test lecture"
        doc = self.create_doc(from_file=static_tim_doc("question_randomization.md"))
        j = self.json_post(
            "/createLecture",
            json_data=dict(
                doc_id=doc.id,
                end_time=end_time,
                lecture_code=lecture_code,
                max_students=50,
                start_time=start_time,
            ),
        )
        lecture_id = j["lecture_id"]
        self.login_test2()
        self.json_post("/joinLecture", query_string={"lecture_id": lecture_id})
        self.login_test1()

        def ask_and_get_answers(parnumber, lecturer_answer, student_answer):
            par_id = doc.document.get_paragraphs()[parnumber].get_id()
            resp = self.json_post(
                "/askQuestion", query_string=dict(doc_id=doc.id, par_id=par_id)
            )
            aid = resp["asked_id"]
            self.json_put(
                "/answerToQuestion",
                json_data={"input": lecturer_answer, "asked_id": aid},
                expect_content=self.ok_resp,
            )
            self.login_test2()
            self.json_put(
                "/answerToQuestion",
                json_data={"input": student_answer, "asked_id": aid},
                expect_content=self.ok_resp,
            )
            self.login_test1()
            resp = self.get("/getLectureAnswers", query_string=dict(asked_id=aid))
            return resp, aid

        # test_shuffle_radio-vertical
        resp, aid = ask_and_get_answers(
            0, [["1", "2", "3", "4"]], [["1", "2", "3", "4"]]
        )
        # lecturer input should not be shuffled, student input should be shuffled
        self.assertEqual([["1", "2", "3", "4"]], resp[0]["answer"])
        self.assertEqual(10, resp[0]["points"])
        self.assertEqual([["4", "1", "3", "5"]], resp[1]["answer"])
        self.assertEqual(13, resp[1]["points"])

        # re-calculate answer to shuffled question correctly on updatePoints call
        self.post(
            "/updatePoints/",
            query_string=dict(asked_id=aid, points="1:-1;2:-2;3:-3;4:-4;5:100"),
        )
        resp = self.get("/getLectureAnswers", query_string=dict(asked_id=aid))
        self.assertEqual(-10, resp[0]["points"])
        self.assertEqual(92, resp[1]["points"])

        # test_shuffle_true-false
        resp, _ = ask_and_get_answers(1, [["1"], ["2"], ["1"]], [["1"], ["2"]])
        self.assertEqual(3, resp[0]["points"])
        self.assertEqual([["1"], ["2"], ["1"]], resp[0]["answer"])
        self.assertEqual(3, resp[0]["points"])
        self.assertEqual([[], ["1"], ["2"]], resp[1]["answer"])
        self.assertEqual(-2, resp[1]["points"])

        # test_shuffle_matrix
        resp, _ = ask_and_get_answers(2, [["1"], ["2"], ["3"]], [["1"], ["3"]])
        self.assertEqual(6, resp[0]["points"])
        self.assertEqual([["1"], ["2"], ["3"]], resp[0]["answer"])
        self.assertEqual(2, resp[1]["points"])
        self.assertEqual([[], ["1"], ["3"]], resp[1]["answer"])

        # test_checkbox-vertical
        resp, _ = ask_and_get_answers(3, [["1", "3"]], [["1", "2"]])
        self.assertEqual(4, resp[0]["points"])
        self.assertEqual([["1", "3"]], resp[0]["answer"])
        self.assertEqual(5, resp[1]["points"])
        self.assertEqual([["2", "3"]], resp[1]["answer"])
