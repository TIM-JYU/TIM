#!/usr/bin/env python3
"""Add the subgroups-manager trans-units to the four locale files.

A fallback for when subgroups-manager-render-and-i18n.patch will not apply
because the .xlf files have drifted. That patch only appends these units before
</body>, so the same result can be reached without patch context.

Idempotent, and safe to run after `npm run extract-i18n` has already added
these units itself: a missing unit is inserted, and a unit that is present but
still carries <target state="new"> - which is what extraction emits - has just
that attribute changed to "translated". Nothing else is touched. Run from the
repository root:

    python3 apply-subgroups-i18n.py            # writes the files
    python3 apply-subgroups-i18n.py --check    # reports, writes nothing

It does NOT touch tim_common/html_sanitize.py, the other half of that patch.
Add "tim-subgroups-manager" to TIM_SAFE_TAGS by hand, next to
"tim-group-dashboard", or the component renders as nothing.
"""

import re
import sys

LOCALES = ("fi", "sv", "es", "it")
FILE = "timApp/i18n/messages.{}.xlf"
ID = re.compile(r'<trans-unit id="([^"]+)"')

UNITS = [
    [
        "5843146741010825721",
        "      <trans-unit id=\"5843146741010825721\" datatype=\"html\">\n        <source>Subgroups</source>\n        <target state=\"translated\">Subgroups</target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">48</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "1809911523755991949",
        "      <trans-unit id=\"1809911523755991949\" datatype=\"html\">\n        <source> No group given. Set the group attribute to the name of the parent group. </source>\n        <target state=\"translated\"> No group given. Set the group attribute to the name of the parent group. </target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">53</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "3821834717032476445",
        "      <trans-unit id=\"3821834717032476445\" datatype=\"html\">\n        <source>Subgroups of <x id=\"START_TAG_STRONG\" ctype=\"x-strong\" equiv-text=\"&lt;strong&gt;\"/><x id=\"INTERPOLATION\" equiv-text=\"&lt;/strong&gt;.\"/><x id=\"CLOSE_TAG_STRONG\" ctype=\"x-strong\" equiv-text=\".&lt;/p&gt;\"/>.</source>\n        <target state=\"translated\">Subgroups of <x id=\"START_TAG_STRONG\" ctype=\"x-strong\" equiv-text=\"&lt;strong&gt;\"/><x id=\"INTERPOLATION\" equiv-text=\"&lt;/strong&gt;.\"/><x id=\"CLOSE_TAG_STRONG\" ctype=\"x-strong\" equiv-text=\".&lt;/p&gt;\"/>.</target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">58,60</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "276841432083534314",
        "      <trans-unit id=\"276841432083534314\" datatype=\"html\">\n        <source>This group has no subgroups.</source>\n        <target state=\"translated\">This group has no subgroups.</target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">92,94</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "5381230309143012956",
        "      <trans-unit id=\"5381230309143012956\" datatype=\"html\">\n        <source> Remove selected </source>\n        <target state=\"translated\"> Remove selected </target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">99</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "7788144676976007748",
        "      <trans-unit id=\"7788144676976007748\" datatype=\"html\">\n        <source> Removing detaches the subgroup from this group. The group itself is not deleted, and members that joined this group through the subgroup remain its members. </source>\n        <target state=\"translated\"> Removing detaches the subgroup from this group. The group itself is not deleted, and members that joined this group through the subgroup remain its members. </target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">102,103</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "1921262188546615038",
        "      <trans-unit id=\"1921262188546615038\" datatype=\"html\">\n        <source>Add subgroups</source>\n        <target state=\"translated\">Add subgroups</target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">108,109</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "4885264227168242478",
        "      <trans-unit id=\"4885264227168242478\" datatype=\"html\">\n        <source>Enter the names of the groups to add as subgroups, one per line.</source>\n        <target state=\"translated\">Enter the names of the groups to add as subgroups, one per line.</target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">116</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "2172902003676402663",
        "      <trans-unit id=\"2172902003676402663\" datatype=\"html\">\n        <source> Refresh </source>\n        <target state=\"translated\"> Refresh </target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">130</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "7762403958505859855",
        "      <trans-unit id=\"7762403958505859855\" datatype=\"html\">\n        <source>Added as subgroups:</source>\n        <target state=\"translated\">Added as subgroups:</target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">136</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "164140394408732146",
        "      <trans-unit id=\"164140394408732146\" datatype=\"html\">\n        <source>Removed from subgroups:</source>\n        <target state=\"translated\">Removed from subgroups:</target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">143</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "5984625725328971623",
        "      <trans-unit id=\"5984625725328971623\" datatype=\"html\">\n        <source>Failed:</source>\n        <target state=\"translated\">Failed:</target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">150</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "1836768055207926526",
        "      <trans-unit id=\"1836768055207926526\" datatype=\"html\">\n        <source>Remove subgroups</source>\n        <target state=\"translated\">Remove subgroups</target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">265,264</context>\n        </context-group>\n      </trans-unit>\n"
    ],
    [
        "5482602639617240100",
        "      <trans-unit id=\"5482602639617240100\" datatype=\"html\">\n        <source>Remove <x id=\"PH\" equiv-text=\"names.length\"/> subgroup(s) from <x id=\"PH_1\" equiv-text=\"group\"/>? The groups themselves are not deleted.</source>\n        <target state=\"translated\">Remove <x id=\"PH\" equiv-text=\"names.length\"/> subgroup(s) from <x id=\"PH_1\" equiv-text=\"group\"/>? The groups themselves are not deleted.</target>\n        <context-group purpose=\"location\">\n          <context context-type=\"sourcefile\">static/scripts/tim/user/subgroups-manager.component.ts</context>\n          <context context-type=\"linenumber\">266,264</context>\n        </context-group>\n      </trans-unit>\n"
    ]
]


def _unit(text: str, uid: str) -> "tuple[int, int] | None":
    """Span of the trans-unit with the given id, or None."""
    start = text.find(f'<trans-unit id="{uid}"')
    if start < 0:
        return None
    end = text.find("</trans-unit>", start)
    return None if end < 0 else (start, end)


def _is_new(text: str, uid: str) -> bool:
    span = _unit(text, uid)
    return span is not None and '<target state="new">' in text[span[0] : span[1]]


def _mark_translated(text: str, uid: str) -> str:
    start, end = _unit(text, uid)
    body = text[start:end].replace(
        '<target state="new">', '<target state="translated">', 1
    )
    return text[:start] + body + text[end:]


def main() -> int:
    check = "--check" in sys.argv[1:]
    failed = False
    for loc in LOCALES:
        path = FILE.format(loc)
        try:
            text = open(path, encoding="utf-8").read()
        except OSError as e:
            print(f"{path}: {e}")
            failed = True
            continue
        present = set(ID.findall(text))
        missing = [(uid, block) for uid, block in UNITS if uid not in present]
        stale = [uid for uid, _ in UNITS if uid in present and _is_new(text, uid)]

        if not missing and not stale:
            print(f"{path}: all {len(UNITS)} units present and translated")
            continue
        if check:
            if missing:
                print(f"{path}: {len(missing)} missing: {[u for u, _ in missing]}")
            if stale:
                print(f'{path}: {len(stale)} still state="new": {stale}')
            failed = True
            continue

        for uid in stale:
            text = _mark_translated(text, uid)
        if missing:
            marker = "    </body>"
            if marker not in text:
                print(f"{path}: no </body> to insert before, skipped")
                failed = True
                continue
            idx = text.rindex(marker)
            text = text[:idx] + "".join(b for _, b in missing) + text[idx:]
        open(path, "w", encoding="utf-8").write(text)
        print(f"{path}: added {len(missing)}, marked translated {len(stale)}")
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
