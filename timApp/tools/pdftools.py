"""
Stamping and merging pdf-files with pdftk and pdflatex.

Visa Naukkarinen
"""

from subprocess import Popen, PIPE, run as subprocess_run
from os import remove, path as os_path
from typing import Union, List
from urllib import request as url_request
from urllib.error import HTTPError

# Default parameter values:
temp_folder_default_path = "/tmp"
# temp_folder_default_path = "static/testpdf"
stamp_model_default_path = "static/tex/stamp_model.tex"
# default format for stamp text
default_stamp_format = "Kokous {date}\n\nLIITE {attachment} lista {issue}"
# how long (seconds) subprocess can take until TimeoutExpired
default_subprocess_timeout = 30
pdfmerge_timeout = 300


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

    def __init__(self, file_path: str = ""):
        """
        :param file_path:
        """
        self.file_path = file_path

    def __str__(self):
        return f"Model stamp missing: {self.file_path}"


class ModelStampInvalidError(PdfError):
    """
    Raised if model tex-file for creating stamps is broken.
    """

    def __init__(self, file_path: str = ""):
        """
        :param file_path:
        """
        self.file_path = file_path

    def __str__(self):
        return f"Model stamp corrupted: {self.file_path}"


class TempFolderNotFoundError(PdfError):
    """
    Raised if the folder for temporary files is missing.
    """

    def __init__(self, folder_path: str = ""):
        """
        :param folder_path:
        """
        self.folder_path = folder_path

    def __str__(self):
        return f"Folder not found: {self.folder_path}"


class AttachmentNotFoundError(PdfError):
    """
    Raised when at least one pdf file in input data is missing.
    """

    def __init__(self, file_path: str = ""):
        """
        :param file_path: path of the attachment pdf that caused the error
        """
        self.file_path = file_path

    def __str__(self):
        return f"Attachment not found: {self.file_path}"


class AttachmentNotAPdfError(PdfError):
    """
    Raised when at least one file in input is not a pdf.
    """

    def __init__(self, file_path: str = ""):
        """
        :param file_path: path of the attachment that caused the error
        """
        self.file_path = file_path

    def __str__(self):
        return f"Attachment not a pdf: {self.file_path}"


class StampFileNotFoundError(PdfError):
    """
    Raised when stamp to use is missing.
    """

    def __init__(self, file_path: str = ""):
        """
        :param file_path: path of the stamp file that caused the error
        """
        self.file_path = file_path

    def __str__(self):
        return f"Stamp-file not foundf {self.file_path}"


class StampDataInvalidError(PdfError):
    """
    Raised if stamp data type is wrong.
    """

    def __init__(self, reason: str = "", item: Union[str, dict, List[dict]] = ""):
        """
        :param reason: error cause
        :param item: item or data that caused the error
        """
        self.reason = reason
        self.item = item

    def __str__(self):
        return f"{self.reason}: {repr(self.item)}"


class StampDataMissingKeyError(PdfError):
    """
    Raised when stamp data is missing one or more required keys.
    """

    def __init__(self, key: str = "", item: Union[str, dict] = ""):
        """
        :param key: The missing key.
        :param item: The dict item which caused the error.
        """
        self.key = key
        self.item = item

    def __str__(self):
        return f"Key {repr(self.key)} not found: {repr(self.item)}"


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
        return f"Error encountered in command: {self.cmd}"


##############################################################################
# Functions:


def merge_pdf(pdf_path_list: List[str], output_path: str) -> str:
    """
    Merges a list of pdfs using pdftk
    :param pdf_path_list: list of pdfs to merge OR
           a string with paths separated with spaces
    :param output_path: merged output file path
    :return: output_path
    """
    for pdf_path in pdf_path_list:
        check_pdf_validity(pdf_path)
    args = ["pdftk"] + pdf_path_list + ["cat", "output", output_path]
    # print(args)
    call_popen(args, pdfmerge_timeout)
    return output_path


def check_pdf_validity(pdf_path: str) -> None:
    """
    Raises error if pdf file doesn't exist or isn't really a pdf.
    :param pdf_path:
    :return:
    """
    if not os_path.exists(pdf_path):
        raise AttachmentNotFoundError(pdf_path)
    if ".pdf" not in pdf_path:
        raise AttachmentNotAPdfError(pdf_path)


def get_stamp_text(item: dict, text_format: str) -> str:
    """
    Gives formatted stamp text; note: may not work properly with non-ascii.
    :param item: Dictionary with 'date','attachment' and 'issue' keys
           or alternatively just 'text'.
    :param text_format: Formatting for filename, meeting date,
           attachment letter and issue/list number.
    :return: Either contents of 'text' key or a formatted string.
    """
    # Normal formatted stamp data takes precedence.
    try:
        return text_format.format(
            file=get_base_filename(item['file']),
            date=item['date'],
            attachment=item['attachment'],
            issue=item['issue'])

    # If stamp data has only a free-form text, use that.
    except KeyError:
        try:
            return item['text']
        # If dictionary doesn't have 'text'-key either;
        # normally this part is obsolete, since checks have been done before.
        except KeyError:
            raise StampDataMissingKeyError('text', item)
    # If input data wasn't a dictionary.
    except TypeError:
        raise StampDataInvalidError("wrong type", item)


def create_stamp(
        model_path: str,
        work_dir: str,
        stamp_name: str,
        text: str,
        remove_pdflatex_files: bool = False) -> str:
    """
    Creates a stamp pdf-file with given text into temp folder.
    :param model_path: Model stamp tex-file's complete path; contains
           '%TEXT_HERE' to locate the text area.
    :param work_dir: The folder where stamp output and temp files will be.
    :param stamp_name: Name of the stamp and temp files (no file extension).
    :param text: Text displayed in the stamp.
    :param remove_pdflatex_files: If true, newly created .aux, .log, .out and
           .tex files will be deleted.
    :return: Complete path of the created stamp pdf-file.
    """
    try:
        stamp_model = open(model_path, "r")

    # Raises custom error if stamp_model is missing.
    except FileNotFoundError:
        raise ModelStampMissingError()

    with stamp_model, open(os_path.join(work_dir, stamp_name + ".tex"),
                           "w+", encoding='utf-8') as stamp_temp:
        try:
            for line in stamp_model:
                if "%TEXT_HERE" in line:
                    stamp_temp.write(text)
                else:
                    stamp_temp.write(line)
        # If stamp_model file is broken.
        # TODO: check if failure to write a new stamp file raises proper error
        except UnicodeDecodeError:
            raise ModelStampInvalidError(model_path)
    args = ["pdflatex", stamp_name]
    # print(args)

    # Directs pdflatex text flood to the log-file pdflatex will create anyway.
    with open(os_path.join(work_dir, stamp_name + ".log"), "a") as pdflatex_log:
        try:
            # Pdflatex can't write files outside of work dir so use cwd.
            rc = subprocess_run(
                args,
                stdout=pdflatex_log,
                cwd=work_dir,
                timeout=default_subprocess_timeout
            ).returncode
            if rc != 0:
                raise SubprocessError(" ".join(args))
        except FileNotFoundError:
            raise SubprocessError(" ".join(args))

    # Optional; delete the files pdflatex created, except the stamp-pdf file,
    # which is obviously needed for stamping.
    if remove_pdflatex_files:
        remove_temp_files(
            work_dir,
            stamp_name,
            [".aux", ".log", ".out", ".tex"])
    return work_dir + stamp_name + ".pdf"


def stamp_pdf(
        pdf_path: str, stamp_path: str, output_path: str,
        remove_stamp: bool = False) -> str:
    """
    Creates a new stamped pdf file (with stamp overlay on each page).
    :param pdf_path:
    :param stamp_path:
    :param output_path:
    :param remove_stamp: Delete stamp file after use.
    :return: output_path
    """
    if not os_path.exists(pdf_path):
        raise AttachmentNotFoundError(pdf_path)
    if not os_path.exists(stamp_path):
        raise StampFileNotFoundError(stamp_path)
    args = ["pdftk", pdf_path, "stamp", stamp_path, "output", output_path]
    # print(args)
    call_popen(args)

    # Optionally clean up the stamp-pdf after use.
    if remove_stamp:
        remove(stamp_path)
    return output_path


def call_popen(args: List[str], timeout_seconds=default_subprocess_timeout) -> None:
    """
    Calls Popen with args list, checks return code and
    raises error if timeouted.
    :param args: List of arguments
    :param timeout_seconds: timeout after which error is raised
    :return: None
    """
    try:
        p = Popen(args, stdout=PIPE)
        stream_data = p.communicate(timeout=timeout_seconds)[0]
        # print(str(stream_data))
        rc = p.returncode
        if rc != 0:
            raise SubprocessError(" ".join(args))
    except FileNotFoundError:
        raise SubprocessError(" ".join(args))


def remove_temp_files(
        dir_path: str, temp_file_name: str, ext_list: List[str]) -> None:
    """
    Deletes temp files created for the stamping process.
    :param dir_path: temp-file folder path
    :param temp_file_name: common part of the names
    :param ext_list: list of extensions after common part for files to remove
    :return: None
    """
    # fail_list = []
    for ext in ext_list:
        try:
            remove(os_path.join(dir_path, temp_file_name + ext))
        # removes the rest of files even if some are missing
        except FileNotFoundError:
            # fail_list.append(path.join(dir_path, temp_file_name + ext))
            continue
    # return fail_list


def check_stamp_data_validity(stamp_data: List[dict]) -> None:
    """
    Raises a specific error if stamp_data is invalid.
    :param stamp_data:
    :return:
    """
    # not a list
    if not isinstance(stamp_data, list):
        raise StampDataInvalidError("is not a list", stamp_data)
    # if empty
    if not stamp_data:
        raise StampDataEmptyError()
    for item in stamp_data:
        # if there are no dictionaries inside the list
        if not isinstance(item, dict):
            raise StampDataInvalidError("is not a dictionary", item)
        # path is always required
        if "file" not in item:
            raise StampDataMissingKeyError("file", item)
        # text or date, attachment & issue are alternatives
        if "text" not in item:
            keys = ["date", "attachment", "issue"]
            for key in keys:
                if key not in item:
                    raise StampDataMissingKeyError(key, item)
                if not item[key]:
                    raise StampDataInvalidError("missing value: " + key, item)
        check_pdf_validity(item["file"])


def is_url(string: str) -> bool:
    """
    Simple test to see if str is an url.
    :param string:
    :return:
    """
    # TODO: Check special cases like ftp?
    if string.startswith("http://") or string.startswith("https://"):
        return True
    else:
        return False


def download_file_from_url(
        url: str,
        output_dir: str = temp_folder_default_path) -> str:
    """
    Downloads a file from url, keeps the filename same.
    :param url: file url
    :param output_dir: download folder
    :return: path of the saved file
    """
    try:
        output_path = os_path.join(output_dir, get_base_filename(url))
        url_request.urlretrieve(url, output_path)
        return output_path
    except HTTPError:
        raise AttachmentNotFoundError(url)


def get_base_filename(path: str, no_extension: bool = False) -> str:
    """
    Returns filename with or without file extension from url or path.
    :param path: url or path to parse
    :param no_extension: keep the extension included
    :return: the file's basename, extension is optional
    """
    if no_extension:
        return os_path.splitext(os_path.basename(path))[0]
    else:
        return os_path.basename(path)


def attachment_params_to_dict(params: List[str]) -> List[dict]:
    """
    Changes list of attachment params to dictionary list that pdftools can use.
    Sets values to right keys and cleans up extra quotes.
    :param params:
    :return:
    """
    if params.__len__() < 6:
        raise StampDataInvalidError("request missing params", ", ".join(params))

    # TODO: More intelligent "-removal in cases of inner quotes.
    attachment = params[3].replace('"', '').replace("'", "").strip()
    issue = params[4].replace('"', '').replace("'", "").strip()

    # If stampformat is empty (as it's set to be if undefined in pareditor.ts), use default.
    stampformat = params[1]
    if not stampformat:
        stampformat = default_stamp_format

    # File path isn't available yet.
    return [{'date': params[0], 'format': stampformat, 'attachment': attachment, 'issue': issue}]


def stamp_pdfs(
        stamp_data: List[dict],
        dir_path: str = temp_folder_default_path,
        stamp_model_path: str = stamp_model_default_path,
        stamp_text_format: str = default_stamp_format) -> List[str]:
    """
    Creates stamps and stamps pdf-files.
    :param stamp_data: dict-list containing pdf-names and stamp-contents
    :param dir_path: folder for temp files
    :param stamp_model_path: tex-file to be used as model for stamps
    :param stamp_text_format: formatting for stamp text, with
            file=0, date=1, attachment=2 and issue=3 keys
    :return: list of stamped pdf paths
    """
    # TODO: Create a new class for stamp_data.

    stamped_pdfs = []

    # creates a new stamp and stamps the corresponding pdfs based on
    # the data-item in dictionary
    # check if temp-folder exists
    if not (os_path.isdir(dir_path) and os_path.exists(dir_path)):
        raise TempFolderNotFoundError(dir_path)

    # check if model stamp exists
    if not os_path.exists(stamp_model_path):
        raise ModelStampMissingError(stamp_model_path)

    # checks multiple potential problems and raises error if invalid
    check_stamp_data_validity(stamp_data)

    for item in stamp_data:
        # names and paths of new files to use as params
        item_basename = get_base_filename(item['file'], True)
        item_stamp_name_no_ext = item_basename + "_stamp"
        item_stamp_path = os_path.join(dir_path, item_stamp_name_no_ext + ".pdf")
        item_stamped_name = item_basename + "_stamped.pdf"
        item_stamped_path = os_path.join(dir_path, item_stamped_name)

        # set
        create_stamp(stamp_model_path,
                     dir_path,
                     item_stamp_name_no_ext,
                     get_stamp_text(item, stamp_text_format),
                     remove_pdflatex_files=True)

        # set to remove stamp-pdf after use
        stamp_pdf(item['file'],
                  item_stamp_path,
                  item_stamped_path,
                  remove_stamp=True)

        stamped_pdfs.append(item_stamped_path)

    return stamped_pdfs
