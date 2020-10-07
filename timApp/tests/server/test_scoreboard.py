"""Server tests for scoreboard."""

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class ScoreboardTest(TimRouteTest):
    def test_scoreboard(self):
        self.login_test1()
        d = self.create_doc()
        r = self.get(d.url, as_tree=True)
        self.assert_js_variable(r, 'score_infos', None)
        d.document.set_settings({'show_scoreboard': True})
        r = self.get(d.url, as_tree=True)
        self.assert_js_variable(r, 'score_infos', [])
        d.document.add_text("""
#- {#t plugin=textfield}
pointsRule:
 maxPoints: 1
        """)
        r = self.get(d.url, as_tree=True)
        summd2 = self.get_js_variable(r, 'score_infos')[0]
        self.assertEqual(d.id, summd2['doc']['id'])
        self.assertEqual(1, summd2['maxTotal'])
        self.assertEqual(0, summd2['total'])
        self.assertEqual([{'fragId': 't', 'maxPoints': 1.0, 'points': 0, 'taskName': 't'}], summd2['tasks'])
        d.document.add_text("""
#- {#t2 plugin=textfield}
pointsRule:
 maxPoints: 2

#- {#t3 plugin=textfield}
pointsRule:
 maxPoints: '3.5'

#- {#t4 plugin=textfield}
pointsRule:
 maxPoints: 0.5

#- {#t5 plugin=textfield}
pointsRule:
 maxPoints: asd_not_number
                """)
        r = self.get(d.url, as_tree=True)
        summd = self.get_js_variable(r, 'score_infos')[0]
        self.assertEqual(d.id, summd['doc']['id'])
        self.assertEqual(7, summd['maxTotal'])
        self.assertEqual(0, summd['total'])
        self.assertEqual([
            {'fragId': 't', 'maxPoints': 1.0, 'points': 0, 'taskName': 't'},
            {'fragId': 't2', 'maxPoints': 2, 'points': 0, 'taskName': 't2'},
            {'fragId': 't3', 'maxPoints': 3.5, 'points': 0, 'taskName': 't3'},
            {'fragId': 't4', 'maxPoints': 0.5, 'points': 0, 'taskName': 't4'},
        ], summd['tasks'])
        d2 = self.create_doc(initial_par="""
#- {#s plugin=textfield}
pointsRule:
 maxPoints: 4

#- {#t plugin=textfield}
pointsRule:
 maxPoints: 1
        """)

        d.document.add_setting('scoreboard_docs', [
            d2.short_name,
        ])
        r = self.get(d.url, as_tree=True)
        summs = self.get_js_variable(r, 'score_infos')
        self.assertEqual(1, len(summs))
        summd2 = summs[0]
        self.assertEqual(d2.id, summd2['doc']['id'])
        self.assertEqual(5, summd2['maxTotal'])
        self.assertEqual(0, summd2['total'])
        self.assertEqual([
            {'fragId': 's', 'maxPoints': 4, 'points': 0, 'taskName': 's'},
            {'fragId': 't', 'maxPoints': 1, 'points': 0, 'taskName': 't'},
        ], summd2['tasks'])
        d.document.add_setting('scoreboard_docs', [
            d.short_name,
            d2.short_name,
        ])
        r = self.get(d.url, as_tree=True)
        summs = self.get_js_variable(r, 'score_infos')
        self.assertEqual(summd, summs[0])
        self.assertEqual(summd2, summs[1])

        # Testing user point calculation.

        self.add_answer(d, 't', points=1.0)
        self.add_answer(d, 't4', points=0.25)
        self.add_answer(d, 't3', points=3)  # set higher points to an earlier answer
        self.add_answer(d, 't3', points=2)

        self.add_answer(d2, 't', points=0.5)
        self.add_answer(d2, 's', points=1.5)
        db.session.commit()

        r = self.get(d.url, as_tree=True)
        summs = self.get_js_variable(r, 'score_infos')

        self.assertEqual(3.25, summs[0]['total'])
        self.assertEqual(2, summs[1]['total'])

        d.document.add_setting('point_sum_rule', {'point_count_method': 'max'})

        r = self.get(d.url, as_tree=True)
        summs = self.get_js_variable(r, 'score_infos')

        self.assertEqual(4.25, summs[0]['total'])  # t3 has greater points in an earlier answer
        self.assertEqual(2, summs[1]['total'])

        settings = {
            'point_count_method': 'max',
            'scoreboard': {
                'groups': ['1st', '2nd'],
            },
            'groups': {'1st': 't|t2', '2nd': 't3|t4'},
        }
        d.document.add_setting('point_sum_rule', settings)
        r = self.get(d.url, as_tree=True)
        summs = self.get_js_variable(r, 'score_infos')
        self.assertEqual(
            [{'fragId': 't', 'maxPoints': 3.0, 'points': 1.0, 'taskName': '1st'},
             {'fragId': 't3', 'maxPoints': 4.0, 'points': 3.25, 'taskName': '2nd'}],
            summs[0]['tasks'],
        )
        self.assertEqual(7, summs[0]['maxTotal'])
        self.assertEqual(4.25, summs[0]['total'])

        # Make sure groupless tasks are not included by default.

        settings['groups'] = {'1st': 't|t2', '2nd': 't4'}
        d.document.add_setting('point_sum_rule', settings)
        r = self.get(d.url, as_tree=True)
        summs = self.get_js_variable(r, 'score_infos')
        self.assertEqual(
            [{'fragId': 't', 'maxPoints': 3.0, 'points': 1.0, 'taskName': '1st'},
             {'fragId': 't4', 'maxPoints': 0.5, 'points': 0.25, 'taskName': '2nd'}],
            summs[0]['tasks'],
        )

        settings = {
            'point_count_method': 'max',
            'scoreboard': {
                'groups': ['1st', '2nd', '*'],
            },
            'groups': {'1st': 't|t2', '2nd': 't4'},
        }
        d.document.add_setting('point_sum_rule', settings)
        r = self.get(d.url, as_tree=True)
        summs = self.get_js_variable(r, 'score_infos')
        self.assertEqual(
            [{'fragId': 't', 'maxPoints': 3.0, 'points': 1.0, 'taskName': '1st'},
             {'fragId': 't3', 'maxPoints': 3.5, 'points': 3.0, 'taskName': 't3'},
             {'fragId': 't4', 'maxPoints': 0.5, 'points': 0.25, 'taskName': '2nd'}],
            summs[0]['tasks'],
        )
