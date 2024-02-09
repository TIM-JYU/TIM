import json
from operator import itemgetter
from typing import Any
from unittest import mock
from unittest.mock import Mock

import responses

from timApp.answer.answer import Answer
from timApp.auth.accesstype import AccessType
from timApp.auth.login import create_or_update_user
from timApp.document.docentry import DocEntry
from timApp.messaging.messagelist.listinfo import Channel
from timApp.notification.send_email import sent_mails_in_testing
from timApp.sisu.scim import SISU_GROUP_PREFIX
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.sisu.sisu import call_sisu_assessments
from timApp.tests.server.test_jsrunner import JsRunnerTestBase
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.user import User, UserOrigin, UserInfo
from timApp.user.usercontact import ContactOrigin
from timApp.user.usergroup import UserGroup, DELETED_GROUP_PREFIX, get_admin_group_id
from timApp.util.utils import seq_to_str, get_current_time

a = ("t", "pass")


def add_name_parts(datas):
    for data in datas:
        fullname = data.get("display")
        if fullname is None:
            continue
        fullname = fullname.split(" ")
        data["name"] = {
            "givenName": fullname[0],
            "familyName": fullname[-1],
            "middleName": " ".join(fullname[1:-1]) if len(fullname) > 2 else None,
        }
    return datas


class ScimTest(TimRouteTest):
    def test_scim(self):
        eid = "jy-CUR-8888-teachers"
        display_name = "ITKP102 2021-09-09--2021-12-20: Opettajat"
        self.json_post(
            "/scim/Groups",
            **scim_error("This action requires authentication.", 401),
        )
        self.json_post(
            "/scim/Groups",
            auth=("cat", "dog"),
            **scim_error("Incorrect username or password.", 401),
        )
        self.json_post(
            "/scim/Groups",
            auth=a,
            **scim_error(
                "The request was well-formed but was unable to be followed due to semantic errors."
            ),
        )

        def check_emails(username: str, emails: list[tuple[ContactOrigin, str]]):
            u = User.get_by_name(username)
            db.session.refresh(u)
            self.assert_contacts(u, Channel.EMAIL, emails)
            primary_origin, primary_email = emails[0]
            self.assert_primary_contact(u, Channel.EMAIL, primary_origin, primary_email)

        r = self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": eid,
                "displayName": display_name,
                "members": add_name_parts(
                    [
                        {
                            "value": "sisuuser",
                            "display": "Sisu User",
                            "email": "x@example.com",
                        },
                        {
                            "value": "sisuuser3",
                            "display": "Sisu User 3",
                            "email": "x3@example.com",
                            "workEmail": "x3work@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_status=201,
            expect_contains={
                "displayName": display_name,
                "id": eid,
                "members": [
                    {
                        "$ref": "http://localhost/scim/Users/sisuuser",
                        "display": "Sisu User",
                        "value": "sisuuser",
                    },
                    {
                        "$ref": "http://localhost/scim/Users/sisuuser3",
                        "display": "Sisu User 3",
                        "value": "sisuuser3",
                    },
                ],
                "schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],
            },
        )

        check_emails("sisuuser", [(ContactOrigin.Sisu, "x@example.com")])
        check_emails(
            "sisuuser3",
            [
                (ContactOrigin.Sisu, "x3work@example.com"),
                (ContactOrigin.Sisu, "x3@example.com"),
            ],
        )

        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": eid,
                "displayName": display_name,
                "members": add_name_parts(
                    [
                        {
                            "value": "sisuuser",
                            "display": "Sisu User",
                            "email": "x@example.com",
                        },
                        {
                            "value": "sisuuser3",
                            "display": "Sisu User 3",
                            "email": "x3@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_status=409,
            expect_content={
                "detail": f"Group already exists: {eid}",
                "schemas": ["urn:ietf:params:scim:api:messages:2.0:Error"],
                "status": "409",
            },
        )

        create_stamp = r["meta"]["created"]
        self.assertEqual(create_stamp, r["meta"]["lastModified"])
        group_id = r["id"]
        self.assertEqual(eid, group_id)
        ru = self.get(
            f"/scim/Users/sisuuser",
            auth=a,
        )
        create_stamp_user = ru["meta"]["created"]
        self.assertEqual(
            {
                "displayName": "Sisu User",
                "id": "sisuuser",
                "externalId": "sisuuser",
                "emails": [{"value": "x@example.com"}],
                "meta": {
                    "created": create_stamp_user,
                    "lastModified": create_stamp_user,
                    "location": "http://localhost/scim/Users/sisuuser",
                    "resourceType": "User",
                },
                "schemas": ["urn:ietf:params:scim:schemas:core:2.0:User"],
            },
            ru,
        )

        user_not_found = {
            "detail": "User not found.",
            "schemas": ["urn:ietf:params:scim:api:messages:2.0:Error"],
            "status": "404",
        }
        self.get(
            f"/scim/Users/xxx",
            auth=a,
            expect_status=404,
            expect_content=user_not_found,
        )
        self.json_put(
            f"/scim/Users/xxx",
            auth=a,
            json_data={
                "displayName": "Sisu User",
                "emails": [{"value": "sisuuser@example.com"}],
                "externalId": "sisuuser",
                "userName": "sisuuser",
            },
            expect_status=404,
            expect_content=user_not_found,
        )

        def update_and_get():
            self.json_put(
                f"/scim/Users/sisuuser",
                auth=a,
                json_data={
                    "displayName": "Sisu User",
                    "emails": [{"value": "sisuuser@example.com"}],
                    "externalId": "sisuuser",
                    "userName": "sisuuser",
                },
            )
            return self.get(
                f"/scim/Users/sisuuser",
                auth=a,
                expect_contains={
                    "displayName": "Sisu User",
                    "id": "sisuuser",
                    "emails": [{"value": "sisuuser@example.com"}],
                    "schemas": ["urn:ietf:params:scim:schemas:core:2.0:User"],
                },
            )

        ru2 = update_and_get()
        check_emails("sisuuser", [(ContactOrigin.Sisu, "sisuuser@example.com")])
        self.assertEqual(create_stamp_user, ru2["meta"]["created"])
        new_modified = ru2["meta"]["lastModified"]
        self.assertNotEqual(create_stamp_user, new_modified)
        ru3 = update_and_get()
        check_emails("sisuuser", [(ContactOrigin.Sisu, "sisuuser@example.com")])
        self.assertEqual(ru3["meta"]["lastModified"], new_modified)

        # self.assertIsNone(UserGroup.get_by_name(eid))
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": eid,
                "displayName": display_name,
                "members": [],
            },
            auth=a,
            expect_status=409,
        )
        self.json_put(
            f"/scim/Groups/{group_id}",
            auth=a,
            **scim_error("JSON payload missing."),
        )
        self.json_put(
            f"/scim/Groups/{group_id}",
            json_data={},
            auth=a,
            **scim_error(
                '{"displayName": ["Missing data for required field."], '
                '"externalId": ["Missing data for required field."], "members": '
                '["Missing data for required field."]}'
            ),
        )
        self.json_put(f"/scim/Groups/xxx", auth=a, expect_status=404)
        self.json_put(f"/scim/Groups/group-999", auth=a, expect_status=404)
        self.json_put(f"/scim/Groups/group-xxx", auth=a, expect_status=404)
        self.json_put(
            f"/scim/Groups/{group_id}",
            json_data={
                "externalId": eid,
                "displayName": display_name,
                "members": add_name_parts(
                    [
                        {
                            "display": "Sisu User",
                            "value": "sisuuser",
                            "email": "x@example.com",
                        },
                        {
                            "display": "Sisu User 2",
                            "value": "sisuuser2",
                            "email": "x2@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_content={
                "displayName": display_name,
                "id": group_id,
                "externalId": eid,
                "members": [
                    {
                        "$ref": "http://localhost/scim/Users/sisuuser",
                        "display": "Sisu User",
                        "value": "sisuuser",
                    },
                    {
                        "$ref": "http://localhost/scim/Users/sisuuser2",
                        "display": "Sisu User 2",
                        "value": "sisuuser2",
                    },
                ],
                "meta": {
                    "created": create_stamp,
                    "lastModified": create_stamp,
                    "location": f"http://localhost/scim/Groups/{eid}",
                    "resourceType": "Group",
                },
                "schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],
            },
        )

        check_emails("sisuuser", [(ContactOrigin.Sisu, "x@example.com")])
        check_emails("sisuuser2", [(ContactOrigin.Sisu, "x2@example.com")])

        r = self.get(
            f"/scim/Groups",
            auth=a,
            query_string={"filter": "externalId sw jy-CUR-8888"},
        )
        self.assertEqual(
            {
                "Resources": [
                    {
                        "id": group_id,
                        "externalId": eid,
                        "meta": {
                            "created": create_stamp,
                            "lastModified": create_stamp,
                            "location": f"http://localhost/scim/Groups/{eid}",
                            "resourceType": "Group",
                        },
                    },
                ],
                "schemas": ["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
                "totalResults": 1,
            },
            r,
        )
        r = self.get(
            f"/scim/Groups",
            auth=a,
            query_string={"filter": "asd sw sisu-"},
            **scim_error("Unsupported filter"),
        )
        r = self.get(
            f"/scim/Groups/{group_id}",
            auth=a,
            expect_content={
                "displayName": display_name,
                "id": group_id,
                "externalId": eid,
                "members": [
                    {
                        "$ref": "http://localhost/scim/Users/sisuuser",
                        "display": "Sisu User",
                        "value": "sisuuser",
                    },
                    {
                        "$ref": "http://localhost/scim/Users/sisuuser2",
                        "display": "Sisu User 2",
                        "value": "sisuuser2",
                    },
                ],
                "meta": {
                    "created": create_stamp,
                    "lastModified": create_stamp,
                    "location": f"http://localhost/scim/Groups/{eid}",
                    "resourceType": "Group",
                },
                "schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],
            },
        )
        self.json_delete(f"/scim/Groups/{group_id}", auth=a, expect_status=204)
        self.json_delete(f"/scim/Groups/{group_id}", auth=a, expect_status=404)
        deleted_group = UserGroup.get_by_name(
            f"{DELETED_GROUP_PREFIX}{SISU_GROUP_PREFIX}{eid}"
        )
        self.assertIsNotNone(deleted_group)

        r = self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": eid,
                "displayName": display_name,
                "members": add_name_parts(
                    [
                        {
                            "value": "sisuuser",
                            "display": "Sisu User",
                            "email": "x@example.com",
                        },
                        {
                            "value": "sisuuser3",
                            "display": "Sisu User 3",
                            "email": "x3@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_status=201,
            expect_contains={
                "displayName": display_name,
                "id": group_id,
                "members": [
                    {
                        "$ref": "http://localhost/scim/Users/sisuuser",
                        "display": "Sisu User",
                        "value": "sisuuser",
                    },
                    {
                        "$ref": "http://localhost/scim/Users/sisuuser3",
                        "display": "Sisu User 3",
                        "value": "sisuuser3",
                    },
                ],
                "schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],
            },
        )
        check_emails("sisuuser", [(ContactOrigin.Sisu, "x@example.com")])
        check_emails("sisuuser3", [(ContactOrigin.Sisu, "x3@example.com")])

        self.assertNotEqual(create_stamp, r["meta"]["lastModified"])
        g = UserGroup.get_by_name(f"{DELETED_GROUP_PREFIX}{eid}")
        self.assertIsNone(g)

        last_modified = r["meta"]["lastModified"]

        u3 = User.get_by_name("sisuuser3")
        u3.email = "x3custom@example.com"
        u3.set_emails(["x3work@example.com"], ContactOrigin.Haka)
        db.session.commit()

        self.json_put(
            f"/scim/Groups/{group_id}",
            json_data={
                "externalId": eid,
                "displayName": display_name,
                "members": add_name_parts(
                    [
                        {
                            "value": "sisuuser3",
                            "display": "Sisu User 3",
                            "email": "x3other@example.com",
                            "workEmail": "x3work@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_content={
                "displayName": display_name,
                "id": group_id,
                "externalId": eid,
                "members": [
                    {
                        "$ref": "http://localhost/scim/Users/sisuuser3",
                        "display": "Sisu User 3",
                        "value": "sisuuser3",
                    },
                ],
                "meta": {
                    "created": create_stamp,
                    "lastModified": last_modified,
                    "location": f"http://localhost/scim/Groups/{eid}",
                    "resourceType": "Group",
                },
                "schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],
            },
        )

        # Custom email was primary => still primary
        # Haka email becomes Sisu email
        check_emails(
            "sisuuser3",
            [
                (ContactOrigin.Custom, "x3custom@example.com"),
                (ContactOrigin.Sisu, "x3work@example.com"),
                (ContactOrigin.Sisu, "x3other@example.com"),
            ],
        )

    def test_no_display_in_members(self):
        r = self.json_post(
            "/scim/Groups",
            json_data={
                "schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],
                "externalId": "jy-CUR-4406-jy-studysubgroup-8514-teachers",
                "displayName": "XKV0201 2019-08-12--2019-12-23: Harjoitusryhm\u00e4: Opettajat",
                "members": add_name_parts(
                    [
                        {
                            "value": "someuser1",
                            "display": "Some User",
                            "$ref": "https://timdevs02-5.it.jyu.fi/scim/Users/someuser1",
                            "type": "User",
                            "email": "someuser1@example.com",
                        },
                        {
                            "value": "someuser2",
                            "display": "Some User",
                            "$ref": "https://timdevs02-5.it.jyu.fi/scim/Users/someuser2",
                            "type": "User",
                            "email": "someuser2@example.com",
                        },
                        {
                            "value": "someuser3",
                            "display": "Some User",
                            "$ref": "https://timdevs02-5.it.jyu.fi/scim/Users/someuser3",
                            "type": "User",
                            "email": "someuser3@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_status=201,
            expect_contains={
                "displayName": "XKV0201 2019-08-12--2019-12-23: Harjoitusryhmä: Opettajat",
                "id": "jy-CUR-4406-jy-studysubgroup-8514-teachers",
                "members": [
                    {
                        "$ref": "http://localhost/scim/Users/someuser1",
                        "display": "Some User",
                        "value": "someuser1",
                    },
                    {
                        "$ref": "http://localhost/scim/Users/someuser2",
                        "display": "Some User",
                        "value": "someuser2",
                    },
                    {
                        "$ref": "http://localhost/scim/Users/someuser3",
                        "display": "Some User",
                        "value": "someuser3",
                    },
                ],
                "schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],
            },
        )

    def test_schema_and_id_in_groups_put(self):
        r = self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1236-teachers",
                "displayName": "ITKP106 2019-09-09--2019-12-20: Luento 1: Opettajat",
                "members": add_name_parts(
                    [
                        {
                            "value": "someone",
                            "display": "Sisu User",
                            "email": "someone@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        r.pop("meta")
        r["members"][0]["display"] = "Changed Name"
        r["members"][0]["email"] = "someone@example.com"
        add_name_parts(r["members"])
        self.json_put(
            "/scim/Groups/jy-CUR-1236-teachers",
            json_data=r,
            auth=a,
        )

    def test_email_resolution(self):
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1250-teachers",
                "displayName": "X001 2019-09-09--2019-12-20: Work Email Test",
                "members": add_name_parts(
                    [
                        {
                            "value": "someone",
                            "display": "Sisu User",
                            "email": "sis1usr@example.com",
                            "workEmail": None,
                        },
                        {
                            "value": "someone2",
                            "display": "Sisu User 2",
                            "email": "sis2usr@example.com",
                            "workEmail": "sis2work@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        self.assertIsNotNone(
            User.get_by_email("sis1usr@example.com"),
            "Sisu User must have their personal email assigned",
        )
        self.assertIsNotNone(
            User.get_by_email("sis2work@example.com"),
            "Sisu User 2 must have their work email assigned",
        )
        self.assertIsNone(
            User.get_by_email("sis2usr@example.com"),
            "Sisu User 2 must have their work email assigned",
        )

    def test_duplicate_email(self):
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1234-teachers",
                "displayName": "ITKP102 2019-09-09--2019-12-20: Luento 1: Opettajat",
                "members": add_name_parts(
                    [
                        {
                            "value": "someone",
                            "display": "Sisu User",
                            "email": "zzz@example.com",
                        },
                        {
                            "value": "someone2",
                            "display": "Sisu User",
                            "email": "zzz@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            **scim_error("The users do not have distinct emails."),
        )
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1234-teachers",
                "displayName": "ITKP103 2019-09-09--2019-12-20: Luento 1: Opettajat",
                "members": add_name_parts(
                    [
                        {
                            "value": "someone",
                            "display": "Sisu User",
                            "email": "zzz@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1239-teachers",
                "displayName": "ITKP103 2019-09-09--2019-12-20: Luento 2: Opettajat",
                "members": add_name_parts(
                    [
                        {
                            "value": "someone2",
                            "display": "Sisu User",
                            "email": "zzz@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            **scim_error(
                "Key (email)=(zzz@example.com) already exists. Conflicting username is: someone2"
            ),
        )

        User.create_with_group(
            UserInfo(
                username="xxx@example.com",
                full_name="Some Guy",
                email="xxx@example.com",
                origin=UserOrigin.Email,
            )
        )
        db.session.commit()
        # Email user can be upgraded
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1240-teachers",
                "displayName": "ITKP108 2019-09-09--2019-12-20: Luento 1: Opettajat",
                "members": add_name_parts(
                    [
                        {
                            "value": "bbb",
                            "display": "Sisu User",
                            "email": "xxx@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1241-teachers",
                "displayName": "ITKP109 2019-09-09--2019-12-20: Luento 1: Opettajat",
                "members": add_name_parts(
                    [
                        {
                            "value": "ccc",
                            "display": "Sisu User",
                            "email": "xxx@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            **scim_error(
                "Key (email)=(xxx@example.com) already exists. Conflicting username is: ccc"
            ),
        )

    def test_duplicate_usernames(self):
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1235-teachers",
                "displayName": "ITKP104 2019-09-09--2019-12-20: Luento 1: Opettajat",
                "members": add_name_parts(
                    [
                        {
                            "value": "aaa",
                            "display": "Sisu User",
                            "email": "aaa@example.com",
                        },
                        {
                            "value": "aaa",
                            "display": "Sisu User",
                            "email": "aaa2@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            **scim_error("The users do not have distinct usernames."),
        )

    def test_inconsistent_name(self):
        # self.json_post(
        #     '/scim/Groups',
        #     json_data={
        #         'externalId': 'inconsistent',
        #         'displayName': 'inconsistent',
        #         'members': [
        #             {
        #                 'value': 'aaa',
        #                 'display': 'John Matt Henry Doe',
        #                 'email': 'aaa2@example.com',
        #                 'name': {
        #                     'givenName': 'John',
        #                     'middleName': 'Matt Henryx',
        #                     'familyName': 'Doe',
        #                 },
        #             },
        #         ],
        #     }, auth=a,
        #     **scim_error("The display attribute 'John Matt Henry Doe' is inconsistent with "
        #                  "the name attributes: given='John', middle='Matt Henryx', "
        #                  "family='Doe'."),
        # )
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1235-teachers",
                "displayName": "ITKP105 2019-09-09--2019-12-20: Luento 1: Opettajat",
                "members": [
                    {
                        "value": "aaa",
                        "display": "John Doe",
                        "email": "aaa2@example.com",
                        "name": {
                            "givenName": "John",
                            "middleName": None,
                            "familyName": "Doex",
                        },
                    },
                ],
            },
            auth=a,
            **scim_error(
                "The display attribute 'John Doe' is inconsistent with the name "
                "attributes: given='John', middle='None', family='Doex'."
            ),
        )
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1235-teachers",
                "displayName": "ITKP105 2019-09-09--2019-12-20: Luento 1: Opettajat",
                "members": [
                    {
                        "value": "aaa",
                        "display": "John Doe Matt",
                        "email": "aaa2@example.com",
                        "name": {
                            "givenName": "Matt",
                            "middleName": "John",
                            "familyName": "Doe",
                        },
                    },
                ],
            },
            auth=a,
            **scim_error(
                "The display attribute 'John Doe Matt' is inconsistent with the "
                "name attributes: given='Matt', middle='John', family='Doe'."
            ),
        )
        self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": "jy-CUR-1235-teachers",
                "displayName": "ITKP105 2019-09-09--2019-12-20: Luento 1: Opettajat",
                "members": [
                    {
                        "value": "aaa",
                        "display": "John Matt Doe",
                        "email": "aaa2@example.com",
                        "name": {
                            "givenName": "Matt",
                            "middleName": "John",
                            "familyName": "Doe",
                        },
                    },
                ],
            },
            auth=a,
            expect_status=201,
        )

    def test_potential_groups(self):
        sent_mails_in_testing.clear()
        responsible_teachers = (
            "jy-CUR-4668-responsible-teachers",
            "ITKP102 P1 2019-09-09--2019-12-20: Rooli - responsible-teacher",
            ["urt-1", "urt-2"],
        )
        entries = [
            (
                "jy-CUR-4668-administrative-persons",
                "ITKP102 P1 2019-09-09--2019-12-20: Rooli - administrative-person",
                ["uap-1", "uap-2"],
            ),
            responsible_teachers,
            (
                "jy-CUR-4668-teachers",
                "ITKP102 P1 2019-09-09--2019-12-20: Rooli - teacher",
                ["ut-1", "ut-2"],
            ),
            (
                "jy-CUR-4668-students",
                "ITKP102 P1 2019-09-09--2019-12-20: Kaikki opiskelijat",
                ["us-1", "us-2"],
            ),
            (
                "jy-CUR-4668-jy-studysubgroup-9515-teachers",
                "ITKP102 2019-09-09--2019-12-20: Luento 1: Opettajat",
                ["u9515-t1", "u9515-t2"],
            ),
            (
                "jy-CUR-4668-jy-studysubgroup-9516-teachers",
                "ITKP102 2019-09-09--2019-12-20: Luento 2: Opettajat",
                ["u9516-t1", "u9516-t2"],
            ),
            (
                "jy-CUR-4668-studysubgroup-teachers",
                "ITKP102 P1 2019-09-09--2019-12-20: Opetusryhmien opettajat",
                ["ussg-t1", "ussg-t2"],
            ),
            (
                "jy-CUR-4668-jy-studysubgroup-9515-students",
                "ITKP102 2019-09-09--2019-12-20: Luento 1: Opiskelijat",
                ["u9515-s1", "u9515-s2"],
            ),
            (
                "jy-CUR-4668-jy-studysubgroup-9516-students",
                "ITKP102 2019-09-09--2019-12-20: Luento 2: Opiskelijat",
                ["u9516-s1", "u9516-s2"],
            ),
            (
                "jy-CUR-4668-studysubgroup-students",
                "ITKP102 P1 2019-09-09--2019-12-20: Opetusryhmien Opiskelijat",
                ["ussg-s1", "ussg-s2"],
            ),
        ]
        for external_id, display_name, users in entries:
            self.json_post(
                "/scim/Groups",
                {
                    "externalId": external_id,
                    "displayName": display_name,
                    "members": add_name_parts(
                        [
                            {
                                "value": u,
                                "display": f"User {u}",
                                "email": f"{u}@example.com",
                            }
                            for u in users
                        ]
                    ),
                },
                auth=a,
                expect_status=201,
            )

        # Make sure teachers are correctly assigned
        teachers = [
            "urt-1",
            "urt-2",
            "ut-1",
            "ut-2",
            "u9515-t1",
            "u9515-t2",
            "ussg-t1",
            "ussg-t2",
        ]
        for u in teachers:
            self.assertTrue(
                User.get_by_name(u).is_sisu_teacher, f"User {u} must be a teacher"
            )
        non_teachers = [
            "uap-1",
            "uap-2",
            "us-1",
            "us-2",
            "u9515-s1",
            "u9515-s2",
            "u9516-s1",
            "u9516-s2",
            "ussg-s1",
            "ussg-s2",
        ]
        for u in non_teachers:
            self.assertFalse(
                User.get_by_name(u).is_sisu_teacher, f"User {u} must not be a teacher"
            )

        # Make sure the sisugroup document exists and has proper settings.
        d = DocEntry.find_by_path("groups/2019/itkp102/09/sisugroups")
        self.assertEqual(
            {
                "global_plugin_attrs": {
                    "all": {
                        "sisugroups": "jy-CUR-4668",
                    }
                },
                "macros": {
                    "course": "ITKP102 P1 2019-09-09--2019-12-20",
                },
                "preamble": "sisugroups",
            },
            d.document.get_settings().get_dict().values,
        )
        self.assertEqual(
            {
                ("jy-CUR-4668-administrative-persons", AccessType.owner.value),
                ("jy-CUR-4668-responsible-teachers", AccessType.owner.value),
                ("jy-CUR-4668-teachers", AccessType.owner.value),
                ("jy-CUR-4668-studysubgroup-teachers", AccessType.owner.value),
            },
            {
                (ac.usergroup.external_id.external_id, ac.type)
                for ac in d.block.accesses.values()
            },
        )
        admin = get_admin_group_id()
        self.assertEqual(admin, d.document.get_changelog().entries[0].group_id)
        self.login(username="ut-1")
        self.get(d.url)

        all_groups = [
            {
                "display_name": "ITKP102 P1 2019-09-09--2019-12-20: Rooli - "
                "administrative-person",
                "doc": None,
                "external_id": "jy-CUR-4668-administrative-persons",
                "name": "itkp102-19p1-administrative-persons",
            },
            {
                "display_name": "ITKP102 P1 2019-09-09--2019-12-20: Rooli - responsible-teacher",
                "doc": None,
                "external_id": "jy-CUR-4668-responsible-teachers",
                "name": "itkp102-19p1-responsible-teachers",
            },
            {
                "display_name": "ITKP102 P1 2019-09-09--2019-12-20: Rooli - teacher",
                "doc": None,
                "external_id": "jy-CUR-4668-teachers",
                "name": "itkp102-19p1-teachers",
            },
            {
                "display_name": "ITKP102 P1 2019-09-09--2019-12-20: Kaikki opiskelijat",
                "doc": None,
                "external_id": "jy-CUR-4668-students",
                "name": "itkp102-19p1-students",
            },
            {
                "display_name": "ITKP102 2019-09-09--2019-12-20: Luento 1: Opettajat",
                "doc": None,
                "external_id": "jy-CUR-4668-jy-studysubgroup-9515-teachers",
                "name": "itkp102-190909-luento-1-teachers",
            },
            {
                "display_name": "ITKP102 2019-09-09--2019-12-20: Luento 2: Opettajat",
                "doc": None,
                "external_id": "jy-CUR-4668-jy-studysubgroup-9516-teachers",
                "name": "itkp102-190909-luento-2-teachers",
            },
            {
                "display_name": "ITKP102 P1 2019-09-09--2019-12-20: Opetusryhmien opettajat",
                "doc": None,
                "external_id": "jy-CUR-4668-studysubgroup-teachers",
                "name": "itkp102-19p1-studysubgroup-teachers",
            },
            {
                "display_name": "ITKP102 2019-09-09--2019-12-20: Luento 1: Opiskelijat",
                "doc": None,
                "external_id": "jy-CUR-4668-jy-studysubgroup-9515-students",
                "name": "itkp102-190909-luento-1-students",
            },
            {
                "display_name": "ITKP102 2019-09-09--2019-12-20: Luento 2: Opiskelijat",
                "doc": None,
                "external_id": "jy-CUR-4668-jy-studysubgroup-9516-students",
                "name": "itkp102-190909-luento-2-students",
            },
            {
                "display_name": "ITKP102 P1 2019-09-09--2019-12-20: Opetusryhmien Opiskelijat",
                "doc": None,
                "external_id": "jy-CUR-4668-studysubgroup-students",
                "name": "itkp102-19p1-studysubgroup-students",
            },
        ]
        self.check_potential_groups("urt-1", all_groups)
        self.check_potential_groups("uap-1", all_groups)
        self.check_potential_groups("ussg-t1", all_groups)
        self.check_potential_groups("u9515-s1", [])
        self.check_potential_groups("us-1", [])
        self.check_potential_groups(
            "u9515-t1",
            [
                {
                    "display_name": "ITKP102 2019-09-09--2019-12-20: Luento 1: Opettajat",
                    "doc": None,
                    "external_id": "jy-CUR-4668-jy-studysubgroup-9515-teachers",
                    "name": "itkp102-190909-luento-1-teachers",
                },
                {
                    "display_name": "ITKP102 2019-09-09--2019-12-20: Luento 1: Opiskelijat",
                    "doc": None,
                    "external_id": "jy-CUR-4668-jy-studysubgroup-9515-students",
                    "name": "itkp102-190909-luento-1-students",
                },
            ],
        )

        self.check_no_group_access("us-1", ["jy-CUR-4668-students"])
        self.check_no_group_access(
            "u9515-t1", ["jy-CUR-4668-students", "jy-CUR-4668-teachers"]
        )
        self.check_no_group_access(
            "u9515-t1",
            ["jy-CUR-4668-students", "jy-CUR-4668-jy-studysubgroup-9515-students"],
            no_access_expected=["jy-CUR-4668-students"],
        )

        self.login(username="u9515-t1")
        self.json_post(
            "/sisu/createGroupDocs",
            json_data=[
                {"externalId": "jy-CUR-4668-jy-studysubgroup-9515-students"},
                {
                    "externalId": "jy-CUR-4668-jy-studysubgroup-9515-teachers",
                    "name": "teachers",
                },
            ],
            expect_status=400,
            expect_content='User group must contain at least one digit and one letter and must not have uppercase or special chars: "teachers"',
        )

        r = self.json_post(
            "/sisu/createGroupDocs",
            json_data=[
                {"externalId": "jy-CUR-4668-jy-studysubgroup-9515-students"},
                {
                    "externalId": "jy-CUR-4668-jy-studysubgroup-9515-teachers",
                    "name": "teachers1",
                },
            ],
        )
        r["created"].sort(key=itemgetter("path"))
        self.assertEqual(
            "groups/2019/itkp102/09/itkp102-190909-luento-1-students",
            r["created"][0]["path"],
        )
        self.assertEqual("groups/2019/itkp102/09/teachers1", r["created"][1]["path"])

        r = self.json_post(
            "/sisu/createGroupDocs",
            json_data=[
                {
                    "externalId": "jy-CUR-4668-jy-studysubgroup-9515-students",
                    "name": "students1",
                },
                {
                    "externalId": "jy-CUR-4668-jy-studysubgroup-9515-teachers",
                    "name": "teachers2",
                },
            ],
        )
        r["updated"].sort(key=itemgetter("path"))
        self.assertEqual("groups/2019/itkp102/09/teachers2", r["updated"][1]["path"])
        self.assertEqual("groups/2019/itkp102/09/students1", r["updated"][0]["path"])
        self.assertEqual("teachers2", r["updated"][1]["title"])
        self.assertEqual("students1", r["updated"][0]["title"])
        self.assertEqual([], r["created"])

        r = self.json_post(
            "/sisu/createGroupDocs",
            json_data=[
                {
                    "externalId": "jy-CUR-4668-jy-studysubgroup-9515-students",
                    "name": "students1",
                },
                {
                    "externalId": "jy-CUR-4668-jy-studysubgroup-9515-teachers",
                    "name": "teachers2",
                },
            ],
        )
        self.assertEqual([], r["created"])
        self.assertEqual([], r["updated"])

        d = DocEntry.find_by_path("groups/2019/itkp102/09/students1")
        self.assertEqual(
            {
                ("jy-CUR-4668-administrative-persons", AccessType.owner.value),
                ("jy-CUR-4668-jy-studysubgroup-9515-teachers", AccessType.owner.value),
                ("jy-CUR-4668-responsible-teachers", AccessType.owner.value),
                ("jy-CUR-4668-studysubgroup-teachers", AccessType.owner.value),
                ("jy-CUR-4668-teachers", AccessType.owner.value),
            },
            {
                (ac.usergroup.external_id.external_id, ac.type)
                for ac in d.block.accesses.values()
            },
        )
        self.assertEqual(
            {
                "macros": {
                    "group": "students1",
                    "fields": ["info"],
                    "maxRows": "40em",
                    "sisugroup": "jy-CUR-4668-jy-studysubgroup-9515-students",
                },
            },
            d.document.get_settings().get_dict().values,
        )
        self.assertEqual(admin, d.document.get_changelog().entries[0].group_id)
        self.json_post(
            "/sisu/createGroupDocs",
            json_data=[
                {
                    "externalId": "jy-CUR-4668-jy-studysubgroup-9515-students",
                    "name": "teachers1",
                },
                {
                    "externalId": "jy-CUR-4668-jy-studysubgroup-9515-teachers",
                    "name": "teachers1",
                },
            ],
            expect_status=403,
            expect_content="Item with a same name already exists.",
        )

        # Make sure there won't be duplicate mails for responsible teachers.
        for external_id, display_name, users in [responsible_teachers]:
            self.json_put(
                f"/scim/Groups/{external_id}",
                {
                    "externalId": external_id,
                    "displayName": display_name,
                    "members": add_name_parts(
                        [
                            {
                                "value": u,
                                "display": f"User {u}",
                                "email": f"{u}@example.com",
                            }
                            for u in users
                        ]
                    ),
                },
                auth=a,
            )

        noreply = "no-reply@tim.jyu.fi"
        self.assertEqual(
            [
                {
                    "mail_from": noreply,
                    "msg": "Kurssin ITKP102 Sisussa olevat ryhmät on kopioitu TIMiin. Ne "
                    "löytyvät dokumentista:\n"
                    "\n"
                    "http://localhost/view/groups/2019/itkp102/09/sisugroups\n"
                    "\n"
                    "Dokumentissa on ohjeet ryhmien käyttämiseen TIMissä.\n"
                    "\n"
                    "Tämä viesti tulee kaikille kurssin vastuuopettajille ja "
                    "hallintohenkilöille.",
                    "rcpt": "uap-1@example.com",
                    "reply_to": noreply,
                    "subject": "Kurssin ITKP102 Sisu-ryhmät on kopioitu TIMiin",
                },
                {
                    "mail_from": noreply,
                    "msg": "Kurssin ITKP102 Sisussa olevat ryhmät on kopioitu TIMiin. Ne "
                    "löytyvät dokumentista:\n"
                    "\n"
                    "http://localhost/view/groups/2019/itkp102/09/sisugroups\n"
                    "\n"
                    "Dokumentissa on ohjeet ryhmien käyttämiseen TIMissä.\n"
                    "\n"
                    "Tämä viesti tulee kaikille kurssin vastuuopettajille ja "
                    "hallintohenkilöille.",
                    "rcpt": "uap-2@example.com",
                    "reply_to": noreply,
                    "subject": "Kurssin ITKP102 Sisu-ryhmät on kopioitu TIMiin",
                },
                {
                    "mail_from": noreply,
                    "msg": "Kurssin ITKP102 Sisussa olevat ryhmät on kopioitu TIMiin. Ne "
                    "löytyvät dokumentista:\n"
                    "\n"
                    "http://localhost/view/groups/2019/itkp102/09/sisugroups\n"
                    "\n"
                    "Dokumentissa on ohjeet ryhmien käyttämiseen TIMissä.\n"
                    "\n"
                    "Tämä viesti tulee kaikille kurssin vastuuopettajille ja "
                    "hallintohenkilöille.",
                    "rcpt": "urt-1@example.com",
                    "reply_to": noreply,
                    "subject": "Kurssin ITKP102 Sisu-ryhmät on kopioitu TIMiin",
                },
                {
                    "mail_from": noreply,
                    "msg": "Kurssin ITKP102 Sisussa olevat ryhmät on kopioitu TIMiin. Ne "
                    "löytyvät dokumentista:\n"
                    "\n"
                    "http://localhost/view/groups/2019/itkp102/09/sisugroups\n"
                    "\n"
                    "Dokumentissa on ohjeet ryhmien käyttämiseen TIMissä.\n"
                    "\n"
                    "Tämä viesti tulee kaikille kurssin vastuuopettajille ja "
                    "hallintohenkilöille.",
                    "rcpt": "urt-2@example.com",
                    "reply_to": noreply,
                    "subject": "Kurssin ITKP102 Sisu-ryhmät on kopioitu TIMiin",
                },
            ],
            sorted(sent_mails_in_testing, key=itemgetter("rcpt")),
        )
        self.assertIn(UserGroup.get_teachers_group(), User.get_by_name("urt-1").groups)
        self.assertNotIn(
            UserGroup.get_teachers_group(), User.get_by_name("us-1").groups
        )

    def test_same_display_name(self):
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-CUR-6565-jy-studysubgroup-1234-students",
                "displayName": "ITKP109 2020-09-09--2020-12-20: Opiskelijaryhmä",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["abc"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )

        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-CUR-6565-jy-studysubgroup-1235-students",
                "displayName": "ITKP109 2020-09-09--2020-12-20: Opiskelijaryhmä",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["abc"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        ug = UserGroup.get_by_external_id("jy-CUR-6565-jy-studysubgroup-1235-students")
        self.assertEqual("itkp109-200909-opiskelijaryhma-1", ug.name)

        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-CUR-6565-jy-studysubgroup-1236-students",
                "displayName": "ITKP109 2020-09-09--2020-12-20: Opiskelijaryhmä",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["abc"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        ug = UserGroup.get_by_external_id("jy-CUR-6565-jy-studysubgroup-1236-students")
        self.assertEqual("itkp109-200909-opiskelijaryhma-2", ug.name)

    def test_scim_overlapping_displaynames(self):
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-CUR-4545-teachers",
                "displayName": "ITKP222 2020-09-09--2020-12-20: Rooli - teacher",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["abc"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-CUR-4546-teachers",
                "displayName": "ITKP222 2020-09-19--2020-12-20: Rooli - teacher",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["abc"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        d = DocEntry.find_by_path("groups/2020/itkp222/09/sisugroups")
        self.assertEqual(3, len(d.document.get_paragraphs()))
        self.json_put(
            f"/scim/Groups/jy-CUR-4546-teachers",
            {
                "externalId": "jy-CUR-4546-teachers",
                "displayName": "ITKP222 2020-09-19--2020-12-20: Rooli - teacher",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"Userz {u}z",
                            "email": f"{u}@example.com",
                        }
                        for u in ["abc"]
                    ]
                ),
            },
            auth=a,
        )
        d = DocEntry.find_by_path("groups/2020/itkp222/09/sisugroups")
        self.assertEqual(3, len(d.document.get_paragraphs()))

        self.json_put(
            f"/scim/Groups/jy-CUR-4545-teachers",
            {
                "externalId": "jy-CUR-4545-teachers",
                "displayName": "ITKP222 2020-09-09--2020-12-20: Rooli - teacher",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"Userz {u}z",
                            "email": f"{u}@example.com",
                        }
                        for u in ["abc"]
                    ]
                ),
            },
            auth=a,
        )
        d = DocEntry.find_by_path("groups/2020/itkp222/09/sisugroups")
        self.assertEqual(3, len(d.document.get_paragraphs()))

        self.json_post(
            f"/scim/Groups",
            {
                "externalId": "jy-CUR-4546-administrative-persons",
                "displayName": "ITKP222 2020-09-19--2020-12-20: Rooli - administrative-person",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"Userz {u}z",
                            "email": f"{u}@example.com",
                        }
                        for u in ["abcadmin"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        d = DocEntry.find_by_path("groups/2020/itkp222/09/sisugroups")
        expected_set = {
            "itkp222-200909-teachers",
            "itkp222-200919-teachers",
            "itkp222-200919-administrative-persons",
        }
        rights_set = {x.usergroup.name for x in d.block.accesses.values()}
        self.assertEqual(expected_set, rights_set)
        self.assertEqual(
            expected_set,
            {x.usergroup.name for x in d.block.parent.block.accesses.values()},
        )
        self.assertEqual(
            expected_set,
            {x.usergroup.name for x in d.block.parent.parent.block.accesses.values()},
        )
        year_folder = d.block.parent.parent.parent
        self.assertEqual("2020", year_folder.title)
        self.assertIn(
            "teachers", [x.usergroup.name for x in year_folder.block.accesses.values()]
        )

    def test_scim_auto_user_merge(self):
        def test_case(eid: str, usr1: UserInfo, usr2: UserInfo):
            create_or_update_user(usr1)
            u2 = create_or_update_user(usr2)
            db.session.commit()
            # Ensure personal folders are created
            self.get("/")
            self.login(username=usr1.username)
            self.get("/")
            self.login(username=usr2.username)
            self.get("/")
            self.json_post(
                "/scim/Groups",
                {
                    "externalId": eid,
                    "displayName": "ITKP102 2020-09-09--2020-12-20: Rooli - teacher",
                    "members": add_name_parts(
                        [
                            {
                                "value": usr1.username,
                                "display": f"User {usr1.username}",
                                "email": usr2.email,
                            }
                        ]
                    ),
                },
                auth=a,
                expect_status=201,
            )
            self.assertIsNone(User.get_by_name(usr2.email))
            self.assertIsNotNone(User.get_by_name(f"{usr2.email}_deleted_{u2.id}"))
            self.assertIsNotNone(User.get_by_name(usr1.username))

            self.login(username=usr1.username)
            # Make sure there are no multiple personal folders. Otherwise an exception would be thrown here.
            self.get("/")

        # Basic case: full names match
        test_case(
            "jy-CUR-6666-teachers",
            UserInfo(
                username="korppiguy",
                email="korppiguy@jyu.fi",
                full_name="Korppi Guy",
                origin=UserOrigin.Korppi,
            ),
            UserInfo(
                username="korppiguy@example.com",
                email="korppiguy@example.com",
                full_name="Korppi Guy",
                origin=UserOrigin.Korppi,
            ),
        )

        # Special case: no common info
        test_case(
            "jy-CUR-6667-teachers",
            UserInfo(
                username="someuser",
                email="someuser@jyu.fi",
                full_name="Some User",
                origin=UserOrigin.Sisu,
            ),
            UserInfo(
                username="custom@example.com",
                email="custom@example.com",
                full_name="SomeUserWrong",
                origin=UserOrigin.Korppi,
            ),
        )

    def test_scim_permission_update_on_new_teachers_group(self):
        """Make sure the rights of already activated groups are updated if the teachers group is provisioned later."""
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-CUR-9999-responsible-teachers",
                "displayName": "ITKP102 2020-09-09--2020-12-20: Vastuuopettajat",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["teacher9999"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-CUR-9999-students",
                "displayName": "ITKP102 2020-09-09--2020-12-20: Opiskelijat",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["student9999"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        self.login(username="teacher9999")
        r = self.json_post(
            "/sisu/createGroupDocs",
            json_data=[
                {"externalId": "jy-CUR-9999-students"},
            ],
        )
        path = r["created"][0]["path"]
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-CUR-9999-teachers",
                "displayName": "ITKP102 2020-09-09--2020-12-20: Opettajat",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["teacher99992"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        d = DocEntry.find_by_path(path)
        self.assertEqual(
            {
                ("jy-CUR-9999-responsible-teachers", AccessType.owner.value),
                ("jy-CUR-9999-teachers", AccessType.owner.value),
            },
            {
                (ac.usergroup.external_id.external_id, ac.type)
                for ac in d.block.accesses.values()
            },
        )

    def test_scim_group_manual_member_update(self):
        eid = "jy-CUR-7777-teachers"
        self.json_post(
            "/scim/Groups",
            {
                "externalId": eid,
                "displayName": "ITKP102 2020-09-09--2020-12-20: Rooli - teacher",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["abc"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        abc = User.get_by_name("abc")
        self.assertEqual("abc User", abc.real_name)
        self.assertEqual("User", abc.given_name)
        self.assertEqual("abc", abc.last_name)
        self.assertEqual("User abc", abc.pretty_full_name)
        u, _ = User.create_with_group(
            UserInfo(
                username="anon@example.com",
                full_name="Anon User",
                email="anon@example.com",
                origin=UserOrigin.Email,
            )
        )
        u2, _ = User.create_with_group(
            UserInfo(
                username="mameikal",
                full_name="Matti Meikäläinen",
                email="mameikal@example.com",
                origin=UserOrigin.Korppi,
            )
        )
        self.make_admin(u)
        self.login(username=u.name)
        ug = UserGroup.get_by_external_id("jy-CUR-7777-teachers")
        self.json_post(f"/groups/addmember/{ug.name}", {"names": [u.name, u2.name]})

        # The SCIM routes must not report the manually added users.
        r = self.get(
            f"/scim/Groups/{eid}",
            auth=a,
        )
        self.assertEqual(
            [
                {
                    "$ref": "http://localhost/scim/Users/abc",
                    "display": "User abc",
                    "value": "abc",
                }
            ],
            r["members"],
        )

        self.json_put(
            f"/scim/Groups/{eid}",
            {
                "externalId": eid,
                "displayName": "ITKP102 2020-09-09--2020-12-20: Rooli - teacher",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"Userz {u}z",
                            "email": f"{u}@example.com",
                        }
                        for u in ["abc"]
                    ]
                ),
            },
            auth=a,
        )
        abc = User.get_by_name("abc")
        self.assertEqual("abcz Userz", abc.real_name)
        self.assertEqual("Userz", abc.given_name)
        self.assertEqual("abcz", abc.last_name)
        self.assertEqual("Userz abcz", abc.pretty_full_name)
        # The manually added user should not get deleted on SCIM update.
        ug = UserGroup.get_by_external_id(eid)
        self.assertEqual(3, len(ug.users))

        self.json_post(
            f"/groups/removemember/{ug.name}",
            {"names": ["abc"]},
            expect_status=400,
            expect_content="Cannot remove not-manually-added users from Sisu groups.",
        )
        self.json_post(
            f"/groups/removemember/{ug.name}", {"names": ["anon@example.com"]}
        )
        self.json_post(f"/groups/removemember/{ug.name}", {"names": ["mameikal"]})

    def check_no_group_access(
        self, username: str, externalids: list[str], no_access_expected=None
    ):
        self.login(username=username)
        if no_access_expected is None:
            no_access_expected = externalids
        self.json_post(
            "/sisu/createGroupDocs",
            json_data=[{"externalId": externalid} for externalid in externalids],
            expect_status=403,
            expect_content=f"You don't have access to all the requested groups: {seq_to_str(no_access_expected)}",
        )

    def check_potential_groups(self, uname: str, expected):
        self.login(username=uname)

        r = self.get(
            "/sisu/getPotentialGroups",
        )
        for g in r:
            g.pop("id")
        self.assertEqual(expected, r)

    def test_uuid_externalid(self):
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-f7d67fab-1f2a-4d01-9687-0910f1bbdfda-students",
                "displayName": "MATP211 P1 2019-08-01--2019-12-31: Kaikki opiskelijat",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["korppiguy"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-f7d67fab-1f2a-4d01-9687-0910f1bbdfda-jy-70c7345b-41d7-4e79-9421-3fb8fba1c7ca-students",
                "displayName": "MATP211 P1 2019-08-01--2019-12-31: Tentti yleisenä tenttipäivänä - ilmoittaudu myös Korpissa: Opiskelijat",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["korppiguy"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "otm-0f84661e-e6f5-4b3d-b7f3-b5701348f3b4-students",
                "displayName": "MATA280 P2 2020-12-16--2020-12-16: Kaikki opiskelijat",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["korppiguy"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "otm-0f84661e-e6f5-4b3d-b7f3-b5701348f3b4-otm-039b5509-763e-4509-8e59-f98173f4a2f0-students",
                "displayName": "MATA280 P2 2020-12-16--2020-12-16: Matematiikan ja tilastotieteen tentit 16.12.2020: Opiskelijat",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["korppiguy"]
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        self.json_post(
            "/scim/Groups",
            {
                "externalId": "jy-f7d67fab-1f2a-4d01-x687-0910f1bbdfda-students",
                "displayName": "MATP211 P1 2019-08-01--2019-12-31: Kaikki opiskelijat",
                "members": add_name_parts(
                    [
                        {
                            "value": u,
                            "display": f"User {u}",
                            "email": f"{u}@example.com",
                        }
                        for u in ["korppiguy"]
                    ]
                ),
            },
            auth=a,
            expect_status=422,
            expect_content={
                "detail": "Unexpected externalId format: "
                '"jy-f7d67fab-1f2a-4d01-x687-0910f1bbdfda-students" (displayName: '
                '"MATP211 P1 2019-08-01--2019-12-31: Kaikki opiskelijat")',
                "schemas": ["urn:ietf:params:scim:api:messages:2.0:Error"],
                "status": "422",
            },
        )

    def test_ip_access(self):
        app.config["SCIM_ALLOWED_IP"] = "1.2.3.4"
        self.get(
            "/scim/Groups",
            query_string={"filter": "externalId sw x"},
            **scim_error("IP not allowed: 127.0.0.1", 403),
        )
        self.get(
            "/scim/Groups",
            query_string={"filter": "externalId sw x"},
            headers=[("X-Forwarded-For", "1.2.3.4")],
            auth=a,
        )
        app.config["SCIM_ALLOWED_IP"] = "127.0.0.1"


def scim_error(msg: str, code=422):
    return dict(
        expect_status=code,
        expect_content={
            "detail": msg,
            "schemas": ["urn:ietf:params:scim:api:messages:2.0:Error"],
            "status": str(code),
        },
    )


class SendGradeTestBase(TimRouteTest):
    def check_send_grade_result(
        self,
        grade_params: dict[str, Any],
        expect_content: dict[str, Any],
        mock_sisu_response: dict[str, Any] | None,
        mock_sisu_status=200,
        expect_status=200,
    ):
        if mock_sisu_response is None:
            self.json_post(
                "/sisu/sendGrades",
                grade_params,
                expect_content=expect_content,
                expect_status=expect_status,
            )
            return
        with responses.RequestsMock() as m:
            self.add_sisu_assessments_mock(
                m, grade_params, mock_sisu_response, mock_sisu_status
            )
            self.json_post(
                "/sisu/sendGrades",
                grade_params,
                expect_content=expect_content,
                expect_status=expect_status,
            )

    def add_sisu_assessments_mock(
        self,
        m: responses.RequestsMock,
        grade_params: dict[str, Any],
        mock_sisu_response: dict[str, Any],
        mock_sisu_status=200,
    ):
        m.add(
            "POST",
            f'{app.config["SISU_ASSESSMENTS_URL"]}{grade_params["destCourse"]}',
            body=json.dumps(mock_sisu_response),
            status=mock_sisu_status,
        )


class StrCreditTest(SendGradeTestBase):
    def test_str_credit(self):
        self.login_test1()
        d = self.create_doc()
        self.add_answer(d, "grade", content=5, user=self.test_user_2)
        self.add_answer(d, "credit", content="5", user=self.test_user_2)
        grade_params = {
            "destCourse": "jy-CUR-1234",
            "docId": d.id,
            "partial": False,
            "dryRun": False,
        }
        t = UserGroup.get_teachers_group()
        t.users.append(self.test_user_1)
        ug = UserGroup.create("course1234")
        ug.external_id = ScimUserGroup(external_id="jy-CUR-1234-responsible-teachers")
        ug.users.append(self.test_user_1)
        ug = UserGroup.create("students1234")
        ug.users.append(self.test_user_2)
        d.document.add_setting("group", "students1234")
        ug = UserGroup.get_by_name("students1234")
        ug.admin_doc = self.create_doc().block
        db.session.commit()
        current_date = get_current_time().strftime("%Y-%m-%d")
        with mock.patch(
            "timApp.sisu.sisu.call_sisu_assessments", wraps=call_sisu_assessments
        ) as m:  # type: Mock
            self.check_send_grade_result(
                grade_params,
                {
                    "assessment_errors": [],
                    "default_selection": [],
                    "sent_assessments": [
                        {
                            "completionCredits": "5",
                            "completionDate": current_date,
                            "gradeId": "5",
                            "privateComment": None,
                            "sentGrade": "5",
                            "sentCredit": "5",
                            "user": {
                                "email": "test2@example.com",
                                "id": 3,
                                "name": "testuser2",
                                "real_name": "Test user 2",
                            },
                        }
                    ],
                },
                {"body": {"assessments": {}}},
            )
        m.assert_called_with(
            "jy-CUR-1234",
            json={
                "assessments": [
                    {
                        "userName": "testuser2",
                        "gradeId": "5",
                        "completionDate": current_date,
                        "completionCredits": 5,
                    }
                ],
                "partial": False,
                "dry_run": False,
            },
        )

        # If credits is float, make sure no exception is thrown and an error is reported.
        self.add_answer(d, "credit", content="5.0", user=self.test_user_2)
        db.session.commit()
        self.check_send_grade_result(
            grade_params,
            {
                "assessment_errors": [
                    {
                        "assessment": {
                            "completionCredits": "5.0",
                            "completionDate": current_date,
                            "gradeId": "5",
                            "privateComment": None,
                            "sentCredit": "5",
                            "sentGrade": "5",
                            "user": {
                                "email": "test2@example.com",
                                "id": 3,
                                "name": "testuser2",
                                "real_name": "Test user 2",
                            },
                        },
                        "message": "completionCredits: Invalid value.",
                    }
                ],
                "default_selection": [],
                "sent_assessments": [],
            },
            mock_sisu_response=None,
        )


class SendGradeTest(SendGradeTestBase):
    def test_send_grades(self):
        self.login_test1()
        d = self.create_doc()
        self.test_user_2.answers.append(
            Answer(task_id=f"{d.id}.grade", content=json.dumps({"c": 5}), valid=True)
        )
        self.test_user_2.answers.append(
            Answer(task_id=f"{d.id}.credit", content=json.dumps({"c": 3}), valid=True)
        )
        self.test_user_3.answers.append(
            Answer(task_id=f"{d.id}.grade", content=json.dumps({"c": 4}), valid=True)
        )
        self.test_user_3.answers.append(
            Answer(task_id=f"{d.id}.credit", content=json.dumps({"c": 2}), valid=True)
        )
        db.session.commit()
        grade_params = {
            "destCourse": "jy-CUR-1234",
            "docId": d.id,
            "partial": False,
            "dryRun": False,
        }
        custom_date = "2019-10-29"
        grade_params_compl_date = {
            "destCourse": "jy-CUR-1234",
            "docId": d.id,
            "partial": False,
            "dryRun": False,
            "completionDate": "2019-10-28T22:00:00.000Z",
            "filterUsers": ["testuser2"],
        }
        grade_params_dryrun = {
            "destCourse": "jy-CUR-1234",
            "docId": d.id,
            "partial": False,
            "dryRun": True,
        }
        grade_params_dryrun_filter = {
            "destCourse": "jy-CUR-1234",
            "docId": d.id,
            "partial": False,
            "dryRun": True,
            "filterUsers": ["testuser2"],
        }
        grade_params_custom_group = {
            "destCourse": "jy-CUR-1234",
            "docId": d.id,
            "partial": False,
            "dryRun": False,
            "groups": ["customgroup"],
        }
        self.json_post(
            "/sisu/sendGrades",
            grade_params,
            expect_status=403,
            expect_content="You are not a TIM teacher.",
        )
        t = UserGroup.get_teachers_group()
        t.users.append(self.test_user_1)
        db.session.commit()
        self.json_post(
            "/sisu/sendGrades",
            grade_params,
            expect_status=403,
            expect_content="You are neither a responsible teacher nor an administrative person of the course jy-CUR-1234.",
        )
        ug = UserGroup.create("course1234")
        ug.external_id = ScimUserGroup(external_id="jy-CUR-1234-responsible-teachers")
        ug.users.append(self.test_user_1)
        db.session.commit()
        self.json_post(
            "/sisu/sendGrades",
            grade_params,
            expect_status=400,
            expect_content='The document must have "group" setting that indicates the student group name.',
        )
        ug = UserGroup.create("students1234")
        db.session.commit()
        d.document.add_setting("group", "studentz1234")
        self.json_post(
            "/sisu/sendGrades",
            grade_params,
            expect_status=400,
            expect_content="Usergroup studentz1234 not found.",
        )
        d.document.add_setting("group", "students1234")

        self.json_post(
            "/sisu/sendGrades",
            grade_params,
            expect_content='You do not have access to the group "students1234".',
            expect_status=403,
        )

        self.json_post(
            "/sisu/sendGrades",
            grade_params_custom_group,
            expect_content="Usergroup customgroup not found.",
            expect_status=400,
        )

        ug = UserGroup.get_by_name("students1234")
        ug.admin_doc = self.create_doc().block
        db.session.commit()

        self.check_send_grade_result(
            grade_params,
            {
                "sent_assessments": [],
                "assessment_errors": [],
                "default_selection": [],
            },
            {"body": {"assessments": {}}},
        )

        ug = UserGroup.get_by_name("students1234")
        ug.external_id = ScimUserGroup(external_id="jy-CUR-9999-teachers")
        db.session.commit()

        self.json_post(
            "/sisu/sendGrades",
            grade_params,
            expect_content='The group "students1234" is not a Sisu student group.',
            expect_status=400,
        )

        ug = UserGroup.get_by_name("students1234")
        ug.external_id.external_id = "jy-CUR-9999-students"
        db.session.commit()

        self.json_post(
            "/sisu/sendGrades",
            grade_params,
            expect_content='The associated course id "jy-CUR-9999" of the group "students1234" '
            'does not match the course setting "jy-CUR-1234".',
            expect_status=400,
        )

        ug = UserGroup.get_by_name("students1234")
        ug.external_id.external_id = "jy-CUR-1234-students"
        db.session.commit()

        self.check_send_grade_result(
            grade_params,
            {
                "sent_assessments": [],
                "assessment_errors": [],
                "default_selection": [],
            },
            {"body": {"assessments": {}}},
        )
        ug = UserGroup.get_by_name("students1234")
        ug.users.append(self.test_user_2)
        ug.users.append(self.test_user_3)
        db.session.commit()
        current_date = get_current_time().strftime("%Y-%m-%d")
        self.assertRegex(current_date, r"\d{4}-\d{2}-\d{2}")

        test_2 = {
            "email": "test2@example.com",
            "id": 3,
            "name": "testuser2",
            "real_name": "Test user 2",
        }
        test_3 = {
            "email": "test3@example.com",
            "id": 4,
            "name": "testuser3",
            "real_name": "Test user 3",
        }
        t2id = test_2["id"]
        t3id = test_3["id"]
        self.check_send_grade_result(
            grade_params_dryrun,
            {
                "assessment_errors": [],
                "default_selection": [t2id, t3id],
                "sent_assessments": [
                    {
                        "completionCredits": 3,
                        "completionDate": None,
                        "gradeId": "5",
                        "privateComment": None,
                        "sentGrade": None,
                        "sentCredit": None,
                        "user": test_2,
                    },
                    {
                        "completionCredits": 2,
                        "completionDate": None,
                        "gradeId": "4",
                        "privateComment": None,
                        "sentGrade": None,
                        "sentCredit": None,
                        "user": test_3,
                    },
                ],
            },
            {"body": {"assessments": {}}},
        )

        # Make sure dry run doesn't modify anything
        self.check_send_grade_result(
            grade_params_dryrun,
            {
                "assessment_errors": [],
                "default_selection": [t2id, t3id],
                "sent_assessments": [
                    {
                        "completionCredits": 3,
                        "completionDate": None,
                        "gradeId": "5",
                        "privateComment": None,
                        "sentGrade": None,
                        "sentCredit": None,
                        "user": test_2,
                    },
                    {
                        "completionCredits": 2,
                        "completionDate": None,
                        "gradeId": "4",
                        "privateComment": None,
                        "sentGrade": None,
                        "sentCredit": None,
                        "user": test_3,
                    },
                ],
            },
            {"body": {"assessments": {}}},
        )

        self.check_send_grade_result(
            grade_params_dryrun_filter,
            {
                "assessment_errors": [],
                "default_selection": [t2id],
                "sent_assessments": [
                    {
                        "completionCredits": 3,
                        "completionDate": None,
                        "gradeId": "5",
                        "privateComment": None,
                        "sentGrade": None,
                        "sentCredit": None,
                        "user": test_2,
                    }
                ],
            },
            {"body": {"assessments": {}}},
        )

        self.check_send_grade_result(
            grade_params_compl_date,
            {
                "sent_assessments": [
                    {
                        "completionDate": custom_date,
                        "gradeId": "5",
                        "completionCredits": 3,
                        "privateComment": None,
                        "sentGrade": "5",
                        "sentCredit": 3,
                        "user": test_2,
                    },
                ],
                "assessment_errors": [],
                "default_selection": [],
            },
            {"body": {"assessments": {}}},
        )
        self.verify_answer_content(f"{d.id}.grade", "c", 5, self.test_user_2)
        self.verify_answer_content(f"{d.id}.sentGrade", "c", "5", self.test_user_2)
        self.verify_answer_content(f"{d.id}.sentCredit", "c", 3, self.test_user_2)

        self.check_send_grade_result(
            grade_params_dryrun,
            {
                "assessment_errors": [],
                "default_selection": [t2id, t3id],
                "sent_assessments": [
                    {
                        "completionCredits": 3,
                        "completionDate": custom_date,
                        "gradeId": "5",
                        "privateComment": None,
                        "sentGrade": "5",
                        "sentCredit": 3,
                        "user": test_2,
                    },
                    {
                        "completionCredits": 2,
                        "completionDate": None,
                        "gradeId": "4",
                        "privateComment": None,
                        "sentGrade": None,
                        "sentCredit": None,
                        "user": test_3,
                    },
                ],
            },
            {"body": {"assessments": {}}},
        )

        self.check_send_grade_result(
            grade_params,
            {
                "sent_assessments": [
                    {
                        "completionCredits": 3,
                        "completionDate": current_date,
                        "gradeId": "5",
                        "privateComment": None,
                        "sentGrade": "5",
                        "sentCredit": 3,
                        "user": test_2,
                    },
                    {
                        "completionCredits": 2,
                        "completionDate": current_date,
                        "gradeId": "4",
                        "privateComment": None,
                        "sentGrade": "4",
                        "sentCredit": 2,
                        "user": test_3,
                    },
                ],
                "assessment_errors": [],
                "default_selection": [],
            },
            {"body": {"assessments": {}}},
        )

        self.verify_answer_content(f"{d.id}.grade", "c", 5, self.test_user_2)
        self.verify_answer_content(f"{d.id}.sentGrade", "c", "5", self.test_user_2)
        self.verify_answer_content(f"{d.id}.sentCredit", "c", 3, self.test_user_2)
        self.verify_answer_content(f"{d.id}.grade", "c", 4, self.test_user_3)
        self.verify_answer_content(f"{d.id}.sentGrade", "c", "4", self.test_user_3)
        self.verify_answer_content(f"{d.id}.sentCredit", "c", 2, self.test_user_3)

        self.check_send_grade_result(
            grade_params_dryrun,
            {
                "sent_assessments": [
                    {
                        "completionDate": current_date,
                        "gradeId": "5",
                        "completionCredits": 3,
                        "privateComment": None,
                        "sentGrade": "5",
                        "sentCredit": 3,
                        "user": test_2,
                    },
                    {
                        "completionDate": current_date,
                        "gradeId": "4",
                        "completionCredits": 2,
                        "privateComment": None,
                        "sentGrade": "4",
                        "sentCredit": 2,
                        "user": test_3,
                    },
                ],
                "assessment_errors": [],
                "default_selection": [t2id, t3id],
            },
            {"body": {"assessments": {}}},
        )

        self.check_send_grade_result(
            grade_params,
            {
                "assessment_errors": [
                    {
                        "assessment": {
                            "completionDate": current_date,
                            "gradeId": "4",
                            "completionCredits": 2,
                            "privateComment": None,
                            "sentGrade": "4",
                            "sentCredit": 2,
                            "user": test_3,
                        },
                        "message": "Sisu: Voimassaolevaa opinto-oikeutta ei löytynyt.",
                    },
                ],
                "sent_assessments": [],
                "default_selection": [t2id],
            },
            {
                "body": {
                    "assessments": {
                        "1": {
                            "userName": {
                                "code": 400003,
                                "reason": "Voimassaolevaa opinto-oikeutta ei löytynyt.",
                            }
                        }
                    }
                }
            },
            400,
        )
        grade_params_partial = {
            **grade_params,
            "partial": True,
        }
        self.check_send_grade_result(
            grade_params_partial,
            {
                "sent_assessments": [
                    {
                        "completionDate": current_date,
                        "gradeId": "5",
                        "completionCredits": 3,
                        "privateComment": None,
                        "sentGrade": "5",
                        "sentCredit": 3,
                        "user": test_2,
                    },
                ],
                "assessment_errors": [
                    {
                        "message": "Sisu: Aikaisempi vahvistettu suoritus (HYV, 3 op)",
                        "assessment": {
                            "user": test_3,
                            "completionCredits": 2,
                            "completionDate": current_date,
                            "privateComment": None,
                            "sentGrade": "4",
                            "sentCredit": 2,
                            "gradeId": "4",
                        },
                    },
                ],
                "default_selection": [],
            },
            {
                "body": {
                    "assessments": {
                        "1": {
                            "userName": {
                                "code": 40009,
                                "reason": "Aikaisempi vahvistettu suoritus",
                                "credits": 3,
                                "gradeId": "HYV",
                            }
                        }
                    }
                }
            },
            207,
        )
        self.check_send_grade_result(
            grade_params_partial,
            {
                "error": "Sertifikaatilla ei oikeutta lähettää suorituksia toteutukseen",
            },
            {
                "error": {
                    "code": 40301,
                    "reason": "Sertifikaatilla ei oikeutta lähettää suorituksia toteutukseen",
                }
            },
            mock_sisu_status=403,
            expect_status=400,
        )

        ug = UserGroup.get_by_name("students1234")
        u, _ = User.create_with_group(
            UserInfo(
                username="sisuuser", full_name="Sisu User", email="sisuuser@example.com"
            )
        )
        ug.users.append(u)
        u, _ = User.create_with_group(
            UserInfo(
                username="sisuuser2",
                full_name="Sisu User",
                email="sisuuser2@example.com",
            )
        )
        ug.users.append(u)
        db.session.commit()
        sisuuser = {
            "email": "sisuuser@example.com",
            "id": 5,
            "name": "sisuuser",
            "real_name": "Sisu User",
        }
        sisuuser2 = {
            "email": "sisuuser2@example.com",
            "id": 6,
            "name": "sisuuser2",
            "real_name": "Sisu User",
        }
        self.check_send_grade_result(
            grade_params,
            {
                "assessment_errors": [
                    {
                        "assessment": {
                            "completionCredits": None,
                            "completionDate": None,
                            "gradeId": None,
                            "privateComment": None,
                            "sentGrade": None,
                            "sentCredit": None,
                            "user": sisuuser,
                        },
                        "message": "gradeId: Field may not be null.",
                    },
                    {
                        "assessment": {
                            "completionCredits": None,
                            "completionDate": None,
                            "gradeId": None,
                            "privateComment": None,
                            "sentGrade": None,
                            "sentCredit": None,
                            "user": sisuuser2,
                        },
                        "message": "gradeId: Field may not be null.",
                    },
                ],
                "default_selection": [],
                "sent_assessments": [],
            },
            # The Sisu API sends only one format error.
            mock_sisu_response=None,
            # {'body': {'assessments': {'2': {'gradeId': {'code': 40001, 'reason': 'Virhe lähetyksen muodossa'}}}}},
            mock_sisu_status=400,
            expect_status=200,
        )

        self.check_send_grade_result(
            grade_params_partial,
            {
                "assessment_errors": [
                    {
                        "assessment": {
                            "completionCredits": 2,
                            "completionDate": current_date,
                            "gradeId": "4",
                            "privateComment": None,
                            "sentGrade": "4",
                            "sentCredit": 2,
                            "user": test_3,
                        },
                        "message": "Sisu: Ilmoittautumista toteutukseen ei löytynyt",
                    },
                    {
                        "assessment": {
                            "completionCredits": None,
                            "completionDate": None,
                            "gradeId": None,
                            "privateComment": None,
                            "sentGrade": None,
                            "sentCredit": None,
                            "user": sisuuser,
                        },
                        "message": "gradeId: Field may not be null.",
                    },
                    {
                        "assessment": {
                            "completionCredits": None,
                            "completionDate": None,
                            "gradeId": None,
                            "privateComment": None,
                            "sentGrade": None,
                            "sentCredit": None,
                            "user": sisuuser2,
                        },
                        "message": "gradeId: Field may not be null.",
                    },
                ],
                "default_selection": [],
                "sent_assessments": [
                    {
                        "completionCredits": 3,
                        "completionDate": current_date,
                        "gradeId": "5",
                        "privateComment": None,
                        "sentGrade": "5",
                        "sentCredit": 3,
                        "user": test_2,
                    }
                ],
            },
            {
                "body": {
                    "assessments": {
                        "1": {
                            "userName": {
                                "code": 40002,
                                "reason": "Ilmoittautumista toteutukseen ei löytynyt",
                            }
                        }
                    }
                }
            },
            mock_sisu_status=207,
            expect_status=200,
        )


class SendGradeJSRunnerTest(SendGradeTestBase, JsRunnerTestBase):
    def test_send_grade_jsrunner(self):
        sent_mails_in_testing.clear()
        self.login_test1()
        d = self.create_jsrun(
            """
groups:
  - studentgroup123
fields:
  - grade
  - credit
  - completionDate
destCourse: "jy-CUR-1234"
destCourseName: "Test course"
program: |!!
// These fields must be set for the user in order for sendGradesToSisu to work.
tools.setString("grade", "5");
tools.setString("credit", "5");
tools.setString("completionDate", "2024-02-02");
!!
postprogram: |!!
gtools.sendGradesToSisu({
  users: [ "testuser2" ],
  sendMailTo: [ "testuser1" ]
});
!!
"""
        )

        d.document.add_text(
            """
#- { defaultplugin="textfield" }

{#grade #} Grade  
{#credit #} Credit
{#completionDate #} Completion date        
"""
        )

        ug_d = self.create_doc()
        ug = UserGroup.create("studentgroup123")
        ug.admin_doc = ug_d.block
        self.test_user_2.add_to_group(ug, None, False)
        db.session.commit()

        t = UserGroup.get_teachers_group()
        self.test_user_1.add_to_group(t, None, False)
        db.session.commit()

        self.do_jsrun(
            d,
            expect_status=403,
            expect_content={
                "error": "You are neither a responsible teacher nor an administrative person "
                "of the course jy-CUR-1234."
            },
        )

        ug = UserGroup.create("course1234")
        ug.external_id = ScimUserGroup(external_id="jy-CUR-1234-responsible-teachers")
        ug.users.append(self.test_user_1)
        db.session.commit()

        def add_sisu_ok_mock(m: responses.RequestsMock):
            self.add_sisu_assessments_mock(
                m, {"destCourse": "jy-CUR-1234"}, {"body": {"assessments": {}}}
            )

        def add_sisu_err_mock(m: responses.RequestsMock):
            self.add_sisu_assessments_mock(
                m,
                {"destCourse": "jy-CUR-1234"},
                {
                    "body": {
                        "assessments": {
                            0: {
                                "userName": {
                                    "code": 40002,
                                    "reason": "Ilmoittautumista toteutukseen ei löytynyt",
                                }
                            }
                        }
                    }
                },
                mock_sisu_status=207,
            )

        self.do_jsrun(
            d,
            expect_status=200,
            expect_content={"web": {"output": "", "errors": [], "outdata": {}}},
            init_mock=add_sisu_ok_mock,
        )

        self.assertEqual(len(sent_mails_in_testing), 1)
        msg = sent_mails_in_testing[0]
        self.assertIn(
            'The course "Test course" has 1 new grades sent to Sisu.', msg["msg"]
        )

        sent_mails_in_testing.clear()

        self.do_jsrun(
            d,
            expect_status=200,
            expect_content={"web": {"output": "", "errors": [], "outdata": {}}},
            init_mock=add_sisu_err_mock,
        )

        self.assertEqual(len(sent_mails_in_testing), 1)
        msg = sent_mails_in_testing[0]
        self.assertIn("Grades could not be sent for 1 students", msg["msg"])
