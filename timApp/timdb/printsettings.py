from timdb.models.user import User


class PrintSettings:

    def __init(self):
        return

    #def __init__(self, user: User):
     #   self.user = user
     #   f = user.get_personal_folder()
      #  self.print_settings_doc = f.get_document('Print Settings',
       #                                         create_if_not_exist=True,
        #                                        creator_group_id=user.get_personal_group().id).document)

    @property
    def get_hash(self):
        return "" #TODO: sensible behaviour