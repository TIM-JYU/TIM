"""Due to 03d73c1f728c4ac38c599cc8d6b711b5fe816684, it is always mandatory to terminate a custom multiline YAML value.
This script adds any missing ones.
"""
from yaml import YAMLError

from timApp.document.yamlblock import YamlBlock, BlockEndMissingError, get_code_block_str
from timApp.admin.util import DryrunnableArguments, enum_pars, process_items, create_argparser, get_url_for_match, \
    print_match
from timApp.document.docinfo import DocInfo


def fix_multiline_endings(doc: DocInfo, args: DryrunnableArguments) -> int:
    found = 0
    for d, p in enum_pars(doc):
        if (p.is_setting() and not p.is_reference()) or p.is_plugin():
            try:
                YamlBlock.from_markdown(p.get_expanded_markdown())
            except BlockEndMissingError as e:
                new_md = p.get_markdown().rstrip()
                cb = get_code_block_str(new_md)
                if len(cb) >= 3:
                    new_md = new_md.rstrip(cb).rstrip()
                elif len(cb) > 0:
                    print(f'Too short code block marker encountered at {get_url_for_match(args, d, p)}')
                    continue
                else:
                    pass
                new_md += '\n' + e.end_str + '\n' + cb
                p.set_markdown(new_md)
                try:
                    YamlBlock.from_markdown(p.get_expanded_markdown())
                except YAMLError:
                    print(f'Unable to fix YAML for {get_url_for_match(args, d, p)}')
                    continue
                found += 1
                if not args.dryrun:
                    p.save()
                print_match(args, d, p, f'missing multiline terminator ({e.end_str})')
            except YAMLError as e:
                print(f'Skipping otherwise broken YAML at {get_url_for_match(args, d, p)}')
    return found


if __name__ == '__main__':
    process_items(fix_multiline_endings,
                  create_argparser('Fixes all setting and plugin paragraphs that lack multiline key terminator.'))
