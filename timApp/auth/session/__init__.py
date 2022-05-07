"""
Module for tracking and managing user sessions.

.. note:: Right now, the module requires :ref:`timApp.defaultconfig.SESSIONS_ENABLE` option to be enabled.

A user can have one or more sessions.
A session contains information about user login information and a unique session ID.

Sessions may also be expired, which allows to block the user from accessing documents until
they verify the session again.
"""
