from enum import Enum

from timApp.user.usercontact import ContactOrigin


class UserOrigin(Enum):
    """Indicates how the user originally registered to TIM.

    Only Email, Korppi and Sisu are used so far; the others are speculative.
    """

    Email = 1
    Korppi = 2
    Sisu = 3
    Haka = 4
    OpenID = 5
    OpenIDConnect = 6
    Facebook = 7
    Google = 8
    Twitter = 9

    def to_contact_origin(self):
        if self == UserOrigin.Email:
            return ContactOrigin.Custom
        elif self == UserOrigin.Korppi or self == UserOrigin.Haka:
            return ContactOrigin.Haka
        elif self == UserOrigin.Sisu:
            return ContactOrigin.Sisu
        return ContactOrigin.Custom
