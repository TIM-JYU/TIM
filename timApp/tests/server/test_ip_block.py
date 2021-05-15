"""Server tests for IP blocking."""

import ipaddress

from timApp.auth.accesshelper import get_ipblocklist_path
from timApp.auth.login import test_pws
from timApp.tests.server.timroutetest import TimRouteTest


class IpBlockTest(TimRouteTest):
    def test_ip_block(self):
        self.logout()  # make sure we're logged out
        with self.temp_config({
            'IP_BLOCK_ALLOWLIST': {
                ipaddress.ip_network('234.234.234.0/24'),
            },
            'IP_BLOCK_MESSAGE': 'Sorry, your IP is not allowed.',
        }):
            self.get('/', expect_contains='Sorry, your IP is not allowed.')
            self.login_test1(force=True, expect_status=403, expect_content='IPNotAllowed')
            self.json_post(
                '/emailSignup',
                {'email': 'testing@example.com'})
            self.json_post(
                '/emailSignupFinish',
                {'realname': 'Testing Signup',
                 'email': 'testing@example.com',
                 'token': test_pws[-1],
                 'password': '1234567890',
                 'passconfirm': '1234567890'},
                expect_content='IPNotAllowed',
                expect_status=403,
            )

        with self.temp_config({
            'IP_BLOCK_ALLOWLIST': {
                ipaddress.ip_network('127.0.0.0/24'),
            },
            'IP_BLOCK_MESSAGE': 'Sorry, your IP is not allowed.',
        }):
            r = self.get('/', as_tree=True)
            notifications = r.cssselect('.global-notification')
            self.assertFalse(notifications)
            self.login_test1(force=True)
            d = self.create_doc(initial_par='#- {plugin=textfield #t}')
            self.post_answer('textfield', f'{d.id}.t', user_input={'c': 'x'})

        with self.temp_config({
            'IP_BLOCK_ALLOWLIST': {
                ipaddress.ip_network('234.234.234.0/24'),
            },
            'IP_BLOCK_MESSAGE': 'Sorry, your IP is not allowed.',
        }):
            self.post_answer(
                'textfield',
                f'{d.id}.t',
                user_input={'c': 'x'},
                expect_status=403,
                expect_content='Answering is not allowed from this IP address.',
            )

        with self.temp_config({
            'IP_BLOCK_ALLOWLIST': {
                ipaddress.ip_network('234.234.234.0/24'),
            },
            'IP_BLOCK_MESSAGE': 'Sorry, your IP is not allowed.',
            'IP_BLOCK_LOG_ONLY': True,
        }):
            self.post_answer(
                'textfield',
                f'{d.id}.t',
                user_input={'c': 'x'},
            )

        with get_ipblocklist_path().open('w') as f:
            f.write('127.0.0.1')

        with self.temp_config({
            'IP_BLOCK_ALLOWLIST': {
                ipaddress.ip_network('234.234.234.0/24'),
            },
            'IP_BLOCK_MESSAGE': 'Sorry, your IP is not allowed.',
            'IP_BLOCK_LOG_ONLY': True,
        }):
            self.post_answer(
                'textfield',
                f'{d.id}.t',
                user_input={'c': 'x'},
                expect_status=403,
                expect_content='Answering is not allowed from this IP address.',
            )
