from datetime import timedelta

# It can be useful to offset the time a little to ensure any checks for expired memberships can pass
group_expired_offset = timedelta(seconds=1)
