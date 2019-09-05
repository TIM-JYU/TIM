"""Imports a document that has been exported with /download/<doc_id>?format=json route."""
import json
from argparse import ArgumentParser

from timApp.document.docentry import DocEntry
from timApp.document.docparagraph import DocParagraph
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


def main():
    parser = ArgumentParser(description='Imports a doc')
    parser.add_argument('--file', help='JSON file to import')
    args = parser.parse_args()
    file: str = args.file
    with app.app_context():
        with open(file, 'r', encoding='utf8') as f:
            data = json.load(f)
        if not isinstance(data, list):
            print('File must be a list of JSON objects')
            return
        d = DocEntry.create('imported', owner_group=UserGroup.get_admin_group())
        for p in data:
            d.document.add_paragraph_obj(DocParagraph.from_dict(d.document, p))
        db.session.commit()
    print('Import finished.')


if __name__ == '__main__':
    main()
