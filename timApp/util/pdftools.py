"""
Stamping and merging pdf-files with pdftk and pdflatex.
Visa Naukkarinen
"""
import uuid
from pathlib import Path
from subprocess import Popen, PIPE, run as subprocess_run
from os import remove, path as os_path
from typing import Union, List
from urllib import request as url_request
from urllib.error import HTTPError
from re import escape as re_escape, compile as re_compile
import timApp.plugin.plugin

# Default parameter values:
from timApp.document.docparagraph import DocParagraph
from timApp.util.logger import log_error

temp_folder_default_path = Path("/tmp")
merged_file_folder = temp_folder_default_path / Path("merged_attachments")
stamp_model_default_path = Path("static/tex/stamp_model.tex")
# Default format for stamp text.
default_stamp_format = "Kokous {date}\n\nLIITE {attachment} lista {issue}"
# How long (seconds) subprocess can take until TimeoutExpired.
default_subprocess_timeout = 30
pdfmerge_timeout = 300
# Max char count for 'attachment', 'issue' and 'date' params.
stamp_param_max_length = 40


##############################################################################
# Custom error classes:


class PdfError(Exception):
    """
    Inherited by all the other custom errors pdftools-module uses.
    """


class ModelStampMissingError(PdfError):
    """
    Raised if model tex-file for creating stamps can't be found.
    """

    def __init__(self, file_path: Path = Path("")):
        """
        :param file_path: Path of the missing file.
        """
        self.file_path = file_path

    def __str__(self):
        return f"Model stamp missing: {self.file_path.absolute().as_posix()}"


class ModelStampInvalidError(PdfError):
    """
    Raised if model tex-file for creating stamps is broken.
    """

    def __init__(self, file_path: Path = Path("")):
        """
        :param file_path: Path of the invalid file.
        """
        self.file_path = file_path

    def __str__(self):
        return f"Model stamp corrupted: {self.file_path.absolute().as_posix()}"


class TempFolderNotFoundError(PdfError):
    """
    Raised if the folder for temporary files is missing.
    """

    def __init__(self, folder_path: Path = Path("")):
        """
        :param folder_path: Path of the missing folder.
        """
        self.folder_path = folder_path

    def __str__(self):
        return f"Folder not found: {self.folder_path.absolute().as_posix()}"


class AttachmentNotFoundError(PdfError):
    """
    Raised when at least one pdf file in input data is missing.
    """

    def __init__(self, file_path: str = ""):
        """
        :param file_path: Path of the attachment pdf that caused the error.
        """
        self.file_path = file_path

    def __str__(self):
        return f"Attachment not found: {self.file_path}"


class AttachmentNotAPdfError(PdfError):
    """
    Raised when at least one file in input is not a pdf.
    """

    def __init__(self, file_path: Path = Path("")):
        """
        :param file_path: Path of the attachment that caused the error.
        """
        self.file_path = file_path

    def __str__(self):
        return f"Attachment not a pdf: {self.file_path.absolute().as_posix()}"


class StampFileNotFoundError(PdfError):
    """
    Raised when stamp to use is missing.
    """

    def __init__(self, file_path: Path = Path("")):
        """
        :param file_path: Path of the stamp file that caused the error.
        """
        self.file_path = file_path

    def __str__(self):
        return f"Stamp-file not found: {self.file_path.absolute().as_posix()}"


class StampDataInvalidError(PdfError):
    """
    Raised if stamp data type is wrong.
    """

    def __init__(self, reason="", item=""):
        """
        :param reason: The error cause explanation.
        :param item: Item or data that caused the error.
        """
        self.reason = reason
        self.item = item

    def __str__(self):
        return f"{self.reason}: {repr(self.item)}"


class StampDataMissingAttributeError(PdfError):
    """
    Raised when stamp data is missing one or more required attributes.
    """

    def __init__(self, attribute: str = "", item: str = ""):
        """
        :param attribute: The missing attribute.
        :param item: Item as string.
        """
        self.attribute = attribute
        self.item = item

    def __str__(self):
        return f"Attribute {repr(self.attribute)} not found: {repr(self.item)}"


class StampDataEmptyError(PdfError):
    """
    Raised if input data is an empty list.
    """

    def __str__(self):
        return f"Stamp data missing"


class SubprocessError(PdfError):
    """
    Raised when subprocesses (pdftk, pdflatex, possibly others) return
    error code or otherwise raise exception.
    """

    def __init__(self, cmd: str = ""):
        self.cmd = cmd

    def __str__(self):
        if "pdftk" in self.cmd and " stamp " in self.cmd:
            return f"Stamping process failed: {self.cmd}"
        elif "pdftk" in self.cmd:
            return f"Merging process failed: {self.cmd}"
        elif "pdflatex" in self.cmd:
            return f"Stamp creating process failed: {self.cmd}"
        return f"Error encountered in command: {self.cmd}"


class MergeListEmptyError(PdfError):
    """
    Raised if empty list given to merge.
    """

    def __str__(self):
        return f"No attachments to merge found"


class StampFormatInvalidError(PdfError):
    """
    Raised if stampformat (from TIM-document settings) is incorrectly formatted.
    """

    def __init__(self, stampformat: str = ""):
        self.stampformat = stampformat

    def __str__(self):
        return f"Incorrect stamp format: {self.stampformat}"


##############################################################################
# Data objects:


class Attachment:
    def __init__(self, path: str, macro: str = "unknown", error: str = "", selected: bool = True):
        self.path = path
        self.macro = macro
        self.selected = selected
        self.error = error


    def to_json(self):
        return {'path': self.path, 'macro': self.macro, 'error': self.error, 'selected': self.selected}


class AttachmentStampData:
    """
    Contains data to create stamp for one attachment.
    """

    def __init__(self, file_path: Path = Path(""),
                 date: str = "",
                 attachment: str = "",
                 issue: Union[str, int] = "",
                 text: str = ""):
        """
        Attachment data, for example: '/files/123/liite-1.pdf', '12.3.2018', 'B', '2'.
        :param file_path: Attachment file path.
        :param date: Meeting date.
        :param attachment: Letter to separate attachments in same list.
        :param issue: List/issue number.
        :param text: Alternative for group of date, issue and attachment.
        """

        self.file = file_path
        self.date = date
        self.issue = str(issue).replace('"', '').replace("'", "").strip()
        self.attachment = attachment.replace('"', '').replace("'", "").strip()
        self.text = text

    def __str__(self):
        return f"{{file:'{self.file.absolute().as_posix()}', date:'{self.date}', issue:'{self.issue}', " \
            f"attachment:'{self.attachment}', text:'{self.text}'}}"

    def validate(self):
        """
        Checks stamp data parameters and attachment file validity.
        :return: None.
        """
        # Path is always required.
        if not self.file:
            raise StampDataMissingAttributeError("file", str(self))
        # Text or date, attachment & issue are alternatives.
        if not self.text:
            if not self.date:
                raise StampDataMissingAttributeError("date", str(self))
            if not self.attachment:
                raise StampDataMissingAttributeError("attachment", str(self))
            if not self.issue:
                raise StampDataMissingAttributeError("issue", str(self))
            if len(self.date) > stamp_param_max_length or \
                    len(self.issue) > stamp_param_max_length or \
                    len(self.attachment) > stamp_param_max_length:
                raise StampDataInvalidError("too long parameter", str(self))
        check_pdf_validity(self.file)


##############################################################################
# Functions:

tex_escapes = {
    '&': r'\&',
    '%': r'\%',
    '$': r'\$',
    '#': r'\#',
    '_': r'\_',
    '{': r'\{',
    '}': r'\}',
    '~': r'\textasciitilde{}',
    '^': r'\^{}',
    '\\': r'\textbackslash{}',
    '<': r'\textless{}',
    '>': r'\textgreater{}',
}
tex_escape_re = re_compile(
    '|'.join(re_escape(key) for key in sorted(tex_escapes.keys(), key=lambda item: - len(item))))


def escape_tex(text: str):
    """
    Escapes special characters in a TeX string.
    Idea taken from https://stackoverflow.com/a/25875504.
    :param text: A plain text message.
    :return: The message escaped to appear correctly in LaTeX.
    """
    return tex_escape_re.sub(lambda match: tex_escapes[match.group()], text)


def test_pdf(pdf_path: str, timeout_seconds: int = pdfmerge_timeout) -> str:
    """
    Test pdf-file suitability for pdftk.
    :param pdf_path: Pdf to test.
    :param timeout_seconds: Timeout after which error is raised.
    :return: Return error message (empty string if no error).
    """
    if is_url(pdf_path):
        return "Error: Attachment link is an URL; please upload the file to TIM"
    if not ".pdf" in pdf_path:
        return "Error: Attachment is not a PDF-file"
    if not Path(pdf_path).exists():
        return "Error: file not found; try reuploading it"
    test_output_path = Path(timApp.util.pdftools.temp_folder_default_path) / f"pdftk_test.pdf"
    args = ["pdftk"] + [pdf_path] + ["cat", "output", test_output_path.absolute().as_posix()]
    p = Popen(args, stdout=PIPE, stderr=PIPE)
    out, err = p.communicate(timeout=timeout_seconds)
    return parse_error(err.decode(encoding='utf-8'))


def parse_error(message: str) -> str:
    """
    Shortens known long PDFtk error messages and logs unexpected ones.
    :param message: Error message string from pdftk.
    :return: Pre-written error message.
    """
    if not message:
        return ""
    if "OWNER PASSWORD REQUIRED" in message:
        return "Error: Unable to merge a password protected file; please remove the password requirement (including empty password)"
    elif "Error: Unable to find file" in message or "java.io.FileNotFoundEception" in message:
        return "Error: file not found; merging tool couldn't find the attachment, try reuploading it"
    elif "java.lang.ClassCastException" in message:
        return "Error: file contains errors; try recreating the file with a different tool"
    elif "Server returned HTTP response code: 403 for URL" in message:
        "Error: file URL forbidden; merging tool couldn't find the attachment link, try reuploading it"
    else:
        log_error(f"PDFtk failed to merge an attachment: {message}")
        return message



def merge_pdfs(pdf_path_list: List[str], output_path: Path) -> Path:
    """
    Merges a list of pdfs using pdftk.
    :param pdf_path_list: List of the paths of pdfs to merge.
    :param output_path: Merged output file path.
    :return: output_path
    """
    if not pdf_path_list:
        raise MergeListEmptyError()
    pdf_path_args = []
    for pdf_path in pdf_path_list:
        pdf_path = parse_tim_url(pdf_path)
        check_pdf_validity(Path(pdf_path))
        pdf_path_args += [pdf_path]

    args = ["pdftk"] + pdf_path_args + ["cat", "output", output_path.absolute().as_posix()]
    call_popen(args, pdfmerge_timeout)
    return output_path


def parse_tim_url(par_file: str, domain: str = "tim.jyu.fi") -> str:
    """
    Parses TIM-links to point to the corresponding file on the server.
    Note: Changes in upload folder need to be updated here as well.
    :param par_file: Link from the macro.
    :param domain: The TIM-domain (the part between "http://" and "/files"), different for timdevs.
    :return: TIM upload file path if the link was within TIM, otherwise pass on unchanged.
    """
    # TODO: Getting right domain in local and timdevs-machines?
    if par_file.startswith("/files/"):
        return "/tim_files/blocks" + par_file
    elif par_file.startswith(f"http://{domain}/files"):
        par_file = "/tim_files/blocks" + par_file.replace(f"http://{domain}", "")
    elif par_file.startswith("https:/{domain}/files"):
        par_file = "/tim_files/blocks" + par_file.replace(f"https://{domain}", "")
    return par_file


def get_attachments_from_pars(paragraphs: List[DocParagraph]) -> List[Attachment]:
    """
    Goes through paragraphs and gets attachments from showPdf-macros.
    Checks file validity with pdftk.
    :param paragraphs: Document paragraphs.
    :return: List of pdf paths and whether the list is complete.
    """
    pdf_list = []
    for par in paragraphs:
        if par.is_plugin() and par.get_attr('plugin') == 'showPdf':
            par_plugin = timApp.plugin.plugin.Plugin.from_paragraph(par)
            par_data = par_plugin.values
            par_file = par_data["file"]
            par_file = parse_tim_url(par_file)
            error_message = test_pdf(par_file)
            selected = not error_message
            par_str = str(par)
            # Find macro name and unselect "unknown"- (bugged) and "perusliite"-attachments.
            macro_name = "unknown"
            if "%%liite" in par_str:
                macro_name = "liite"
            elif "%%perusliite" in par_str or "unknown" in par_str:
                macro_name = "perusliite"
                selected = False
            # File check and GUI link require different paths.
            par_file = par_file.replace("tim_files/blocks/", "")
            pdf_list.append(Attachment(par_file, error=error_message, macro=macro_name, selected=selected))
    return pdf_list


def check_pdf_validity(pdf_path: Path) -> None:
    """
    Raises error if pdf file doesn't exist or isn't really a pdf.
    :param pdf_path: Pdf to check.
    :return: None if not interrupted by error.
    """
    if not pdf_path.exists():
        raise AttachmentNotFoundError(pdf_path.absolute().as_posix())
    if ".pdf" not in pdf_path.suffix:
        raise AttachmentNotAPdfError(pdf_path)


def get_stamp_text(item: AttachmentStampData, text_format: str) -> str:
    """
    Gives formatted stamp text; note: may not work properly with non-ascii.
    :param item: AttachmentStampData with 'date','attachment' and 'issue' attributes
           or alternatively just 'text'.
    :param text_format: Formatting for filename, meeting date,
           attachment letter and issue/list number.
    :return: Either contents of 'text' attribute or a formatted string.
    """
    # Normal formatted stamp data takes precedence.
    try:
        return text_format.format(
            file=Path(escape_tex(item.file.absolute().as_posix())).name,
            date=escape_tex(item.date),
            attachment=escape_tex(item.attachment),
            issue=escape_tex(item.issue))

    # If stamp data has only a free-form text, use that.
    # TODO: Untested, because the stamping initiated from TIM doesn't use 'text'.
    except (AttributeError, ValueError, SyntaxError):
        try:
            return item.text
        # If object doesn't have 'text'-attribute either;
        # normally this part is obsolete, since checks have been done before.
        except (AttributeError, ValueError, SyntaxError):
            raise StampDataMissingAttributeError('text', "")

    # If input data wasn't right kind of object.
    except TypeError:
        raise StampDataInvalidError("wrong type", "")
    # If text_format uses numbers.
    except IndexError:
        raise StampFormatInvalidError(text_format)


def create_stamp(
        model_path: Path,
        work_dir: Path,
        stamp_name: str,
        text: str,
        remove_pdflatex_files: bool = False) -> Path:
    """
    Creates a stamp pdf-file with given text into temp folder.
    :param model_path: Model stamp tex-file's complete path; contains
           '%TEXT_HERE' to locate the stamp text area.
    :param work_dir: The folder where stamp output and temp files will be.
    :param stamp_name: Name of the stamp and temp files (no file extension needed).
    :param text: LaTeX-escaped text displayed in the stamp.
    :param remove_pdflatex_files: If true, newly created .aux, .log, .out and
           .tex files will be deleted.
    :return: Complete path of the created stamp pdf-file.
    """
    stamp = work_dir / f"{stamp_name}.tex"
    try:
        with model_path.open("r", encoding='utf-8') as model_file:
            with stamp.open("w+", encoding='utf-8') as stamp_temp_file:
                    for line in model_file:
                        if "%TEXT_HERE" in line:
                            stamp_temp_file.write(line.replace("%TEXT_HERE", text))
                        else:
                            stamp_temp_file.write(line)
            args = ["pdflatex", stamp_name]
    except UnicodeDecodeError: # If stamp_model file is broken.
        raise ModelStampInvalidError(model_path)
    except FileNotFoundError:
        raise ModelStampMissingError()

    # Directs pdflatex text flood to the log-file pdflatex will create anyway.
    pdflatex_log = work_dir / f"{stamp_name}.log"
    with pdflatex_log.open("a", encoding='utf-8') as pdflatex_output:
        try:
            # Pdflatex can't write files outside of the work dir so uses cwd.
            rc = subprocess_run(
                args,
                stdout=pdflatex_output,
                cwd=work_dir.absolute().as_posix(),
                timeout=default_subprocess_timeout
            ).returncode
            if rc != 0:
                raise SubprocessError(" ".join(args))
        except:
            raise SubprocessError(" ".join(args))

    # Optional; deletes the files pdflatex created, except the stamp-pdf file,
    # which is obviously needed for stamping.
    if remove_pdflatex_files:
        remove_temp_files(
            work_dir,
            stamp_name,
            ["aux", "log", "out", "tex"])
    return work_dir / f"{stamp_name}.pdf"


def stamp_pdf(
        pdf_path: Path, stamp_path: Path, output_path: Path,
        remove_stamp: bool = False) -> Path:
    """
    Creates a new stamped pdf file (with stamp overlay on each page).
    :param pdf_path: Path of the pdf to stamp.
    :param stamp_path: Path of the stamp file.
    :param output_path: Path of the new stamped pdf.
    :param remove_stamp: Delete stamp file after use.
    :return: output_path
    """
    if not pdf_path.exists():
        raise AttachmentNotFoundError(pdf_path.absolute().as_posix())
    if not stamp_path.exists():
        raise StampFileNotFoundError(stamp_path)
    args = ["pdftk", pdf_path.absolute().as_posix(),
            "stamp", stamp_path.absolute().as_posix(),
            "output", output_path.absolute().as_posix()]
    call_popen(args)

    # Optionally clean up the stamp-pdf after use.
    if remove_stamp:
        remove(stamp_path.absolute().as_posix())
    return output_path


def call_popen(args: List[str], timeout_seconds=default_subprocess_timeout) -> None:
    """
    Calls Popen with args list, checks return code and
    raises error if timeouted.
    :param args: List of arguments.
    :param timeout_seconds: Timeout after which error is raised.
    :return: None.
    """
    try:
        p = Popen(args, stdout=PIPE, stderr=PIPE)
        out, err = p.communicate(timeout=timeout_seconds)
        rc = p.returncode
        if rc != 0:
            raise SubprocessError(err.decode(encoding='utf-8'))
    except FileNotFoundError:
        raise SubprocessError(" ".join(args))


def remove_temp_files(
        dir_path: Path, temp_file_name: str, ext_list: List[str]) -> None:
    """
    Deletes temp files created for the stamping process.
    :param dir_path: Temp-file folder path.
    :param temp_file_name: Common part of the names.
    :param ext_list: List of extensions (after the common part) for files to remove.
    :return: None.
    """
    for ext in ext_list:
        try:
            file_to_remove = dir_path / f"{temp_file_name}.{ext}"
            remove(file_to_remove.absolute().as_posix())
        # Removes the rest of files even if some are missing.
        except FileNotFoundError:
            continue


def check_stamp_data_validity(stamp_data_list: List[AttachmentStampData]) -> None:
    """
    Raises a specific error if stamp_data is invalid.
    :param stamp_data_list: List of objects containing the stamp data.
    :return: None, but will raise error if something invalid.
    """
    # If empty list.
    if not stamp_data_list:
        raise StampDataEmptyError()
    for item in stamp_data_list:
        item.validate()


def is_url(string: str) -> bool:
    """
    Simple test to see if str is an url.
    :param string: String to test.
    :return: True if url, false if not.
    """
    # TODO: Check special cases like ftp?
    if string.startswith("http://") or string.startswith("https://"):
        return True
    else:
        return False


def download_file_from_url(
        url: str,
        output_dir: Path = temp_folder_default_path) -> Path:
    """
    Downloads a file from url, keeps the filename same.
    :param url: File url.
    :param output_dir: Download folder.
    :return: Path of the saved file.
    """
    try:
        output_path = output_dir / os_path.basename(url)
        url_request.urlretrieve(url, output_path.absolute().as_posix())
        return output_path
    except HTTPError:
        raise AttachmentNotFoundError(url)


def create_new_tex_file(content: str, folder: Path = temp_folder_default_path) -> Path:
    """
    Creates tex-file with random name and input content.
    :param content: LaTeX content in string.format.
    :param folder: Folder where new file will be added.
    :return: Path-object for the new file.
    """
    path = folder / f'{uuid.uuid4()}.tex'
    with path.open("w", encoding='utf-8') as file:
        file.write(content)
    return path


def stamp_pdfs(
        stamp_data: List[AttachmentStampData],
        dir_path: Path = temp_folder_default_path,
        stamp_model_path: Path = stamp_model_default_path,
        stamp_text_format: str = default_stamp_format) -> List[Path]:
    """
    Creates new stamps and stamps the corresponding pdfs based on
    the data in a list of AttachmentStampData objects.
    :param stamp_data: List of objects containing pdf-paths and stamp-attributes.
    :param dir_path: Folder for temp files.
    :param stamp_model_path: Tex-file to be used as model for stamps.
    :param stamp_text_format: Formatting for stamp text, with attributes:
            file, date, attachment and issue.
    :return: List of stamped pdf paths.
    """
    stamped_pdfs = []

    # Check if temp-folder exists and is a folder:
    if not (dir_path.exists() and dir_path.is_dir()):
        raise TempFolderNotFoundError(dir_path)
    # Check if model stamp exists.
    if not stamp_model_path.exists():
        raise ModelStampMissingError(stamp_model_path)
    # Checks multiple potential problems and raises error if invalid.
    check_stamp_data_validity(stamp_data)

    for item in stamp_data:
        # Names and paths of new files to use as params.
        item_basename = item.file.stem
        item_stamp_name_no_ext = item_basename + "_stamp"
        item_stamp_path = dir_path / f"{item_stamp_name_no_ext}.pdf"
        item_stamped_name = item_basename + "_stamped.pdf"
        item_stamped_path = dir_path / item_stamped_name

        create_stamp(stamp_model_path,
                     Path(dir_path),
                     item_stamp_name_no_ext,
                     get_stamp_text(item, stamp_text_format),
                     remove_pdflatex_files=True)
        stamp_pdf(Path(item.file),
                  item_stamp_path,
                  item_stamped_path,
                  remove_stamp=True)

        stamped_pdfs.append(item_stamped_path)

    return stamped_pdfs

def restamp_pdfs(
        stamp_data: List[AttachmentStampData],
        stamp_model_path: Path = stamp_model_default_path,
        stamp_text_format: str = default_stamp_format) -> List[Path]:
    """
    Update stamps for a list of attachments.
    :param stamp_data: List of objects containing pdf-paths (non-stamped) and stamp-attributes.
    :param stamp_model_path: Tex-file to be used as model for stamps.
    :param stamp_text_format: Formatting for stamp text, with attributes:
            file, date, attachment and issue.
    :return: List of stamped pdf paths.
    """
    stamped_pdfs = []
    # Check if model stamp exists.
    if not stamp_model_path.exists():
        raise ModelStampMissingError(stamp_model_path)
    # Checks multiple potential problems and raises error if invalid.
    check_stamp_data_validity(stamp_data)

    for item in stamp_data:
        current_attachment_folder = item.file.parent
        # Check if current folder exists and is a folder:
        if not (current_attachment_folder.exists() and current_attachment_folder.is_dir()):
            raise TempFolderNotFoundError(current_attachment_folder)


        # Names and paths of new files to use as params.
        item_basename = item.file.stem
        item_stamp_name_no_ext = item_basename + "_stamp"
        item_stamp_path = current_attachment_folder / f"{item_stamp_name_no_ext}.pdf"
        item_stamped_name = item_basename + "_stamped.pdf"
        item_stamped_path = current_attachment_folder / item_stamped_name

        create_stamp(stamp_model_path,
                     Path(current_attachment_folder),
                     item_stamp_name_no_ext,
                     get_stamp_text(item, stamp_text_format),
                     remove_pdflatex_files=True)
        stamp_pdf(Path(item.file),
                  item_stamp_path,
                  item_stamped_path,
                  remove_stamp=True)

        stamped_pdfs.append(item_stamped_path)

    return stamped_pdfs
