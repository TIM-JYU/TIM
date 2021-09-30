"""Helper script for quickly doing most mechanical refactorings required when converting
an AngularJS component to an Angular component."""

import re
import sys
from dataclasses import dataclass


@dataclass
class TypeScriptSrcEditor:
    src: str

    def add_imports(self, s):
        imp = r"\n?(import \{)"
        self.regex_replace(imp, s + r"\n\1", count=1)

    def delete_line(self, s):
        self.src = re.sub(f" *{re.escape(s)}.*\n", "", self.src)

    def replace(self, f, t):
        self.src = self.src.replace(f, t)

    def search(self, s):
        m = re.search(s, self.src, re.DOTALL)
        return m.groups(1)[0] if m else None

    def search_all(self, s):
        yield from re.findall(s, self.src, re.DOTALL)

    def regex_replace(self, pat, to, flags=0, count=0):
        self.src = re.sub(pat, to, self.src, count=count, flags=flags)


def main():
    path = sys.argv[1]
    if not path.endswith(".ts"):
        print("not a ts file")
        return
    with open(path, encoding="utf8") as f:
        src = f.read()
    s = TypeScriptSrcEditor(src)
    is_dialog = "extends DialogController" in src
    s.add_imports(
        """
import {Component} from "@angular/core";"""
    )
    if is_dialog:
        s.add_imports(
            """
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";"""
        )
        s.delete_line("import {DialogController} from ")
        s.replace("extends DialogController", "extends AngularDialogComponent")
        s.regex_replace(r"{params: (\w+|{\w+: \w+})}", r"\1")
        s.replace("extends DialogController", "extends AngularDialogComponent")
        s.replace("static component", "protected dialogName")

    if is_dialog:
        comp_name_orig = s.search(r'protected dialogName = "(\w+)"( as const)?;')
        comp_name = re.sub(r"Dialog$", "", comp_name_orig)
        module_var = None
    else:
        comp_name_orig = s.search(r'\w+\.component\("(\w+)", (.|\n)+\)')
        comp_name = comp_name_orig
        module_var = s.search(r'(\w+)\.component\("\w+", (.|\n)+\)')
    comp_name = re.sub(r"^tim", "", comp_name)

    if comp_name_orig != comp_name:
        s.replace(comp_name_orig, comp_name)

    comp_selector = re.sub(r"(?<!^)(?=[A-Z])", "-", comp_name).lower()

    template = s.search("template: `(.+)`")
    if not template:
        template_url = s.search('templateUrl: "(.+)"').strip("/")
        with open(template_url, encoding="utf8") as f:
            template = f.read()

    suffix = "-dialog" if is_dialog else ""
    s.regex_replace(
        "\n(export )?class",
        f'\n@Component({{selector: "tim-{comp_selector}{suffix}", template: `{template}`}})\nexport class',
    )

    s.replace("$onInit", "ngOnInit")
    s.replace("$doCheck", "ngDoCheck")
    s.replace(", IScope", "")
    s.replace("IController, ", "")
    s.delete_line("import * as focusMe")
    s.regex_replace(r"\nmarkAsUsed\(.+\);", "")
    s.replace("markAsUsed, ", "")
    if is_dialog:
        s.delete_line("super.ngOnInit();")
        s.delete_line('static $inject = ["$element", "$scope"] as const;')
        s.replace("resolve.params", "data")

        s.regex_replace(r"\nregisterDialogComponent\((.|\n)+\}\);", "")
        s.delete_line("import {registerDialogComponent,")

        s.regex_replace(
            r"""
            (?P<ret>return )?
            (await )?
            showDialog\(\s*
              (?P<comp>\w+),\s*
                 \{\s*(params:\s*\(\)\s*=>\s*
                     (?P<param>\w+|\({\w+}\))(\s*,\s*)?)?\}(\s*,\s*)?
                   (\{[^}]*\})?\s*
            \)\s*
                 (?P<res>.result)?(?P<comma>;)?""",
            r"\g<ret>(await angularDialog.open(\g<comp>, \g<param>))\g<res>\g<comma>",
            flags=re.VERBOSE | re.DOTALL,
        )
        s.replace("protected dismiss()", "dismiss()")
        s.delete_line("import {showDialog} from ")
        s.regex_replace(
            r"<dialog-header>(.|\n)*</dialog-header>",
            """<ng-container header>
                {{ getTitle() }}
            </ng-container>""",
        )
        for part in ["body", "footer"]:
            s.replace(f"<dialog-{part}>", f"<ng-container {part}>")
            s.replace(f"</dialog-{part}>", "</ng-container>")
        s.regex_replace(r"export function (show|open)", r"export async function \1")
    else:
        s.regex_replace(rf"\n{module_var}\.component\((.|\n)+\}}\);", "")
        s.delete_line(f"import {{{module_var}}}")
        s.delete_line("static $inject")
        s.delete_line("private scope: IScope;")
        s.delete_line("this.scope = scope;")
        s.regex_replace("(, )?scope: IScope", "")
        s.replace(" implements IController", "")
        s.delete_line("import {IController}")

    s.regex_replace(
        r"constructor\(protected element: JQLite, protected scope: IScope\) \{",
        "constructor() {",
    )
    s.replace("super(element, scope);", "super();")
    s.delete_line('import {IScope} from "angular";')
    s.delete_line("// noinspection JSUnusedGlobalSymbols")
    s.delete_line("// noinspection JSUnusedLocalSymbols")

    # collect all controller references
    controller_vars = list(s.search_all(r"\$ctrl\.(\w+)"))
    if is_dialog:
        controller_vars.append("getTitle")

    # fix template syntax
    s.replace("$ctrl.", "")
    s.regex_replace(r"\btim-dialog\b", "tim-dialog-frame")
    s.replace("ng-click", "(click)")
    s.replace("ng-if", "*ngIf")
    s.replace("ng-show", "*ngIf")
    s.regex_replace(r'ng-hide="(\w+)(\.\w+)?"', r'*ngIf="!\1\2"')
    s.replace("ng-model", "[(ngModel)]")
    s.replace("ng-disabled", "[disabled]")
    s.replace("ng-class", "[ngClass]")
    s.replace("ng-style", "[ngStyle]")
    s.replace("ng-keypress", "(keypress)")
    s.replace("ng-keydown", "(keydown)")
    s.replace('tooltip-enable="', '[isDisabled]="!')
    s.replace("uib-tooltip=", "tooltip=")
    s.replace("uib-dropdown-menu", "*dropdownMenu")
    s.replace("uib-dropdown-toggle", "dropdownToggle")
    s.replace("uib-dropdown", "dropdown")
    s.replace("uib-typeahead", "[typeahead]")
    s.replace("typeahead-min-length", "[typeaheadMinLength]")
    s.replace("tim-error-state", "timErrorState")
    s.replace("{{::", "{{")
    s.replace('="::', '="')
    s.replace("ng-bind-html", "[innerHtml]")
    s.replace("ng-href", "href")
    s.replace("ng-value", "[value]")
    s.replace("ng-bind", "[innerText]")
    s.replace("tim-location", "timLocation")
    s.replace("tim-short-name", "timShortName")
    s.replace("ng-change", "(ngModelChange)")
    s.replace("ng-checked", "[checked]")
    s.replace("ng-mouseover", "(mouseover)")
    s.replace("<tim-error-message for=", "<tim-error-message [for]=")
    s.regex_replace(r'ng-hide="!(\w+)"', r'*ngIf="\1"')
    s.regex_replace(r'focus-me="\w+"', "focusMe")
    s.regex_replace(r'<form name="(\w+)"', r"<form #\1")

    for var in controller_vars:
        s.regex_replace(f"(private|protected) (async )?({var})", r"\2\3")

    s.regex_replace(fr'ng-repeat="(\w+) in ', r'*ngFor="let \1 of ')

    with open(path, encoding="utf8", mode="wt", newline="\n") as f:
        f.write(s.src)


if __name__ == "__main__":
    main()
