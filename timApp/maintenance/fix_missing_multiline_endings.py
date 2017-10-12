"""Due to 03d73c1f728c4ac38c599cc8d6b711b5fe816684, it is always mandatory to terminate a custom multiline YAML value.
This script adds any missing ones.
"""
from yaml import YAMLError

from timApp.documentmodel.yamlblock import YamlBlock, BlockEndMissingError, get_code_block_str
from timApp.maintenance.util import BasicArguments, enum_pars, process_items, create_argparser, get_str, print_match
from timApp.timdb.docinfo import DocInfo


def fix_multiline_endings(doc: DocInfo, args: BasicArguments):
    found = 0
    for d, p in enum_pars(doc):
        if (p.is_setting() and not p.is_reference()) or p.is_plugin():
            try:
                YamlBlock.from_markdown(p.get_markdown())
            except BlockEndMissingError as e:
                new_md = p.get_markdown().rstrip()
                cb = get_code_block_str(new_md)
                if len(cb) >= 3:
                    new_md = new_md.rstrip(cb).rstrip()
                elif len(cb) > 0:
                    print(f'Too short code block marker encountered at {get_str(d, p)}')
                    continue
                else:
                    pass
                new_md += '\n' + e.end_str + '\n' + cb
                try:
                    YamlBlock.from_markdown(new_md)
                except YAMLError:
                    print(f'Unable to fix YAML for {get_str(d, p)}')
                    continue
                found += 1
                if not args.dryrun:
                    p.set_markdown(new_md)
                    p.save()
                print_match(args, d, p, f'missing multiline terminator ({e.end_str})')
    return found


if __name__ == '__main__':
    process_items(fix_multiline_endings,
                  create_argparser('Fixes all setting and plugin paragraphs that lack multiline key terminator.'))
