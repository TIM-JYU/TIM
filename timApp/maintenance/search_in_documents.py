import sys

from timApp.maintenance.util import enum_pars
from timApp.tim_app import app


def search(term: str):
    found = 0
    for d, p in enum_pars():
        md = p.get_exported_markdown(skip_tr=True)
        header = f"{d.url}#{p.get_id()}"
        if term in md:
            found += 1
            print(f"""
{header}
{'-' * len(header)}
{md}
""".strip() + "\n")
    print(f'Found {found} paragraphs.')


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Search term missing.')
        sys.exit(1)
    search_term = sys.argv[1]
    with app.test_request_context():
        search(search_term)
