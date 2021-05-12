from typing import List

from timApp.admin.util import enum_pars, process_items, create_argparser, DryrunnableArguments
from timApp.document.docinfo import DocInfo


def find_nested_areas(d: DocInfo, _args: DryrunnableArguments) -> int:
    active_areas: List[str] = []
    for _, p in enum_pars(d):
        area = p.get_attr('area')
        if area:
            if active_areas:
                print(f'nested areas: {d.path}')
                return 1
            active_areas.append(area)
        area_end = p.get_attr('area_end')
        if area_end:
            try:
                active = active_areas.pop()
            except IndexError:
                print(f'missing area start: {d.path}')
                return 1
            else:
                if active != area_end:
                    print(f'missing area start: {d.path}')
    if active_areas:
        print(f'missing area end: {d.path}')
        return 1
    return 0


if __name__ == '__main__':
    process_items(find_nested_areas,
                  create_argparser('Finds documents that have nested or broken areas.'))
