from tim_app import db
from timdb.dbutils import insert_block
from timdb.blocktypes import blocktypes
from timdb.models import docentry


class GamificationDocument(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'gamificationdocument'
    id = db.Column (db.Integer, db.ForeignKey('block.id'), primary_key=True)

    g_doc = None # type: GamificationDocument

    docentry.create(id)

    @staticmethod
    def create(id) -> 'GamificationDocument':
        """Creates a new entry into GamificationDocument table"""

        g_doc_id = insert_block(id, blocktypes.GAMIFICATIONDOC, commit=False)
        gamification_doc = GamificationDocument(g_doc_id)
        gamification_doc.create()

        gamificationdocument = GamificationDocument(id=g_doc_id)
        db.session.add(gamificationdocument)
        db.session.commit()
        gamificationdocument.g_doc = gamification_doc
        return gamificationdocument