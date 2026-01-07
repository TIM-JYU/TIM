import html
from typing import Optional

from timApp.document.exceptions import ValidationException

OptionalParId = Optional[str]


class ValidationIssue:
    def __init__(self, par_id: OptionalParId):
        self.par_id = par_id

    def __str__(self):
        if self.par_id is not None:
            return f"{self.issue_name} noticed in paragraph {self.par_id}"
        else:
            return f"{self.issue_name} noticed in a paragraph"

    @property
    def issue_name(self):
        return "Issue"


class AreaIssue(ValidationIssue):
    def __init__(self, par_id: OptionalParId, area_name: str):
        super().__init__(par_id)
        self.area_name = area_name

    def __str__(self):
        area_message = f" for area '{self.area_name}'" if self.area_name else ""
        if self.par_id is not None:
            return (
                f"{self.issue_name} noticed{area_message} in paragraph {self.par_id}."
            )
        else:
            return f"{self.issue_name} noticed{area_message}."

    @property
    def issue_name(self):
        return "Area issue"


class DuplicateTaskId(ValidationIssue):
    def __init__(self, par_id: OptionalParId, task_id: str):
        super().__init__(par_id)
        self.task_id = task_id

    @property
    def issue_name(self):
        return f"Duplicate task id '{self.task_id}'"


class AreaEndWithoutStart(AreaIssue):
    @property
    def issue_name(self):
        return "Area end without start "


class DuplicateAreaEnd(AreaIssue):
    @property
    def issue_name(self):
        return "Duplicate area end"


class AreaWithoutEnd(AreaIssue):
    @property
    def issue_name(self):
        return "Area without end"


class MultipleAreasWithSameName(AreaIssue):
    @property
    def issue_name(self):
        return "Multiple areas with same name"


class OverlappingClassedArea(AreaIssue):
    def __init__(self, par_id: OptionalParId, area_name: str, second_area: str):
        super().__init__(par_id, area_name)
        self.second_area = second_area

    @property
    def issue_name(self):
        return "Overlapping classed area"


class ZeroLengthArea(AreaIssue):
    @property
    def issue_name(self):
        return "Zero-length area"


class AttributesAtEndOfCodeBlock(ValidationIssue):
    @property
    def issue_name(self):
        return "Attributes at end of code block"


class InvalidParagraphId(ValidationIssue):
    @property
    def issue_name(self):
        return "Invalid paragraph id"


class DuplicateParagraphId(ValidationIssue):
    @property
    def issue_name(self):
        return "Duplicate paragraph id"


class ValidationResult:
    def __init__(self):
        self.issues = []

    def add_issue(self, issue: ValidationIssue):
        str_issue = str(issue).rstrip(".,")
        # Check for existing issue of the same start
        for idx, i in enumerate(self.issues):
            str_i = str(i).rstrip(".,")
            if str_issue.startswith(str_i):
                self.issues[idx] = issue
                return
        self.issues.append(issue)

    def has_issue(self, issue: type):
        return self.has_any_issue(issue)

    def has_any_issue(self, *issues: type):
        return any(isinstance(i, j) for j in issues for i in self.issues)

    def __str__(self):
        if not self.issues:
            return ""
        if len(self.issues) == 1:
            return str(self.issues[0])
        items = "".join(f"<li>{html.escape(str(i))}</li>" for i in self.issues)
        return f"Errors: <ol>{items}</ol>"

    @property
    def has_critical_issues(self):
        return self.has_any_issue(
            DuplicateParagraphId, AttributesAtEndOfCodeBlock, InvalidParagraphId
        )

    def raise_if_has_critical_issues(self):
        if self.has_critical_issues:
            raise ValidationException(str(self))

    def raise_if_has_any_issues(self):
        if self.issues:
            raise ValidationException(str(self))
