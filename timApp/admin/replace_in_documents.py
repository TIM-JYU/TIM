from argparse import ArgumentTypeError
from typing import Tuple, Generator

import attr
from yaml import YAMLError

from timApp.admin.search_in_documents import create_basic_search_argparser, search, SearchResult, \
    SearchArgumentsBasic
from timApp.admin.util import process_items, DryrunnableOnly, BasicArguments, get_url_for_match
from timApp.document.docinfo import DocInfo
from timApp.document.yamlblock import YamlBlock
from timApp.timdb.exceptions import TimDbException


@attr.s
class ReplaceArguments(DryrunnableOnly, SearchArgumentsBasic):
    """Arguments for a replacement operation."""
    to: str = attr.ib(kw_only=True)


@attr.s
class ReplaceArgumentsCLI(ReplaceArguments, BasicArguments):
    """Command-line arguments for a replacement operation."""


def min_replacement_length(x: str) -> str:
    if len(x) < 3:
        raise ArgumentTypeError("String to replace must be at least 3 characters.")
    return x


@attr.s
class ReplacementResult:
    """Represents a single replacement in a :class:`DocParagraph`.

    """
    search_result: SearchResult = attr.ib(kw_only=True)
    replacement: str = attr.ib(kw_only=True)
    error: str = attr.ib(kw_only=True, default=None)

    def get_new_markdown(self) -> str:
        """Gets the new markdown after applying the replacement string.

        NOTE: This replaces ALL occurrences currently.
        """
        old_md = self.search_result.match.string
        new_md = self.search_result.match.re.sub(self.replacement, old_md, count=0)
        return new_md

    def get_replacement(self) -> Tuple[str, str]:
        return self.search_result.match.group(0), self.search_result.match.expand(self.replacement)

    def get_replacement_desc(self) -> str:
        f, t = self.get_replacement()
        return f'{f} -> {t}'

    def format_match(self, args: ReplaceArgumentsCLI) -> str:
        r = self.search_result
        m = r.match
        gps = tuple((m.group(0), *m.groups()))

        return args.format.format(
            *gps,
            doc_id=r.doc.id,
            par_id=r.par.get_id(),
            url=get_url_for_match(args, r.doc, r.par),
            to=self.get_replacement()[1]
        )


def perform_replace(d: DocInfo, args: ReplaceArguments) -> Generator[ReplacementResult, None, None]:
    """Performs a search-and-replace operation for the specified document, yielding ReplacementResults.

    :param args: The replacement arguments. If args.dryrun is True, no actual replacement will occur.
    :param d: The document to process.
    """
    for r in search(d, args, use_exported=False):
        repl = ReplacementResult(search_result=r, replacement=args.to)
        old_md = r.par.get_markdown()
        new_md = repl.get_new_markdown()
        if r.par.is_yaml():
            try:
                yb = YamlBlock.from_markdown(r.par.get_expanded_markdown())
            except YAMLError:
                repl.error = f'YAML is invalid before replacement, so not doing anything'
            except TimDbException as e:
                repl.error = f'Exception: {str(e)}'
            if not repl.error:
                try:
                    p_temp = r.par.clone()
                    p_temp.set_markdown(new_md)
                    yb_new = YamlBlock.from_markdown(p_temp.get_expanded_markdown())
                except YAMLError:
                    repl.error = 'YAML would be invalid after replacement, so not doing anything'
        yield repl
        if not args.dryrun and not repl.error:
            # The method get_new_markdown replaces all occurrences in a paragraph,
            # so some ReplacementResults are (in this sense) redundant.
            if old_md == new_md:
                continue
            r.par.set_markdown(new_md)
            r.par.clear_cache()
            r.par.save()


def replace_and_print(d: DocInfo, args: ReplaceArgumentsCLI) -> int:
    """Same as :func:`perform_replace`, but prints the matches according to the provided format."""
    n = 0
    for r in perform_replace(d, args):
        n = r.search_result.num_pars_found
        if r.error:
            print(r.error + ':')
        print(f'{r.format_match(args)}')
    return n


if __name__ == '__main__':
    parser = create_basic_search_argparser('Replaces strings in documents', is_readonly=False)
    parser.add_argument('--to', help=r'The replacement string. Use \1, \2, ... for referencing regex groups.')
    process_items(replace_and_print, parser)
