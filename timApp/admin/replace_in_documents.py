from argparse import ArgumentTypeError
from copy import copy
from dataclasses import dataclass
from typing import Tuple, Generator, Optional, Union

import attr
from yaml import YAMLError

from timApp.admin.search_in_documents import create_basic_search_argparser, search, SearchResult, \
    SearchArgumentsBasic
from timApp.admin.util import process_items, DryrunnableOnly, BasicArguments, get_url_for_match
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.viewcontext import default_view_ctx
from timApp.document.yamlblock import YamlBlock
from timApp.timdb.exceptions import TimDbException


@attr.s
class ReplaceArguments(DryrunnableOnly, SearchArgumentsBasic):
    """Arguments for a replacement operation."""
    to: Optional[str] = attr.ib(kw_only=True, default=None)
    add_attr: Optional[str] = attr.ib(kw_only=True, default=None)


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


@dataclass
class AttrModification:
    search_result: SearchResult
    par: DocParagraph
    name: str
    value: str
    was_already: bool
    mod_type: str = 'add'

    @property
    def error(self) -> Optional[str]:
        return None

    def format_match(self, args: ReplaceArgumentsCLI) -> str:
        r = self.search_result
        url = get_url_for_match(args, r.doc, r.par)
        if self.mod_type == 'add':
            action = 'existing' if self.was_already else 'added'
        else:
            raise NotImplementedError
        return f'{url}: {action} attribute {self.name}="{self.value}"'


def perform_replace(
        d: DocInfo,
        args: ReplaceArguments,
) -> Generator[Union[ReplacementResult, AttrModification], None, None]:
    """Performs a search-and-replace operation for the specified document, yielding ReplacementResults.

    :param args: The replacement arguments. If args.dryrun is True, no actual replacement will occur.
    :param d: The document to process.
    """
    for r in search(d, args, use_exported=False):
        repl: Union[None, ReplacementResult, AttrModification] = None
        old_md = None
        new_md = None
        mi = r.par.doc.get_settings().get_macroinfo(default_view_ctx)
        if args.to is not None:
            repl = ReplacementResult(search_result=r, replacement=args.to)
            old_md = r.par.get_markdown()
            new_md = repl.get_new_markdown()
            if r.par.is_yaml():
                try:
                    yb = YamlBlock.from_markdown(r.par.get_expanded_markdown(mi))
                except YAMLError:
                    repl.error = f'YAML is invalid before replacement, so not doing anything'
                except TimDbException as e:
                    repl.error = f'Exception: {str(e)}'
                if not repl.error:
                    try:
                        p_temp = r.par.clone()
                        p_temp.set_markdown(new_md)
                        yb_new = YamlBlock.from_markdown(p_temp.get_expanded_markdown(mi))
                    except YAMLError:
                        repl.error = 'YAML would be invalid after replacement, so not doing anything'
            yield repl
        old_attrs = copy(r.par.get_attrs())
        if args.add_attr is not None:
            attr_name, attr_value = args.add_attr.split('=')
            repl = AttrModification(
                search_result=r,
                par=r.par,
                name=attr_name,
                value=attr_value,
                was_already=old_attrs.get(attr_name) == attr_value,
            )
            yield repl
            r.par.set_attr(attr_name, attr_value)
        if not repl:
            raise Exception('--to or --add-attr must be given')
        if not args.dryrun and not repl.error:
            # The method get_new_markdown replaces all occurrences in a paragraph,
            # so some ReplacementResults are (in this sense) redundant.
            if old_md == new_md and old_attrs == r.par.get_attrs():
                continue
            if new_md is not None:
                r.par.set_markdown(new_md)
            # TODO currently DocParagraph.store doesn't take self.attrs into account; it only saves __data dict.
            #  So for now we just modify the dict reference directly.
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
    group_action = parser.add_mutually_exclusive_group(required=True)
    group_action.add_argument('--to', help=r'The replacement string. Use \1, \2, ... for referencing regex groups.')
    group_action.add_argument('--add-attr', help='An attribute to add to the matching paragraphs.')
    process_items(replace_and_print, parser)
