import os
import shlex
from contracts import contract
import gitpylib.repo
import gitpylib.file
import gitpylib.common
import gitpylib.sync
from timdb.timdbbase import TimDbException

class NothingToCommitException(Exception):
    pass

#TODO: This should possibly be a class.

def customCommit(files, msg, author, skip_checks=False, include_staged_files=False):
    """Record changes in the local repository.

  Args:
    files: the files to commit.
    msg: the commit message.
    skip_checks: if the pre-commit hook should be skipped or not. (Defaults to
      False.)
    include_staged_files: whether to include the contents of the staging area in
      the commit or not. (Defaults to False.)

  Returns:
    the output of the commit command.
  """
    cmd = 'commit {0} --author="{1} <>" -m{2}'.format('--no-verify ' if skip_checks else '', author, shlex.quote(msg))
    if not files and include_staged_files:
        return gitpylib.common.safe_git_call(cmd)[0]

    return gitpylib.common.safe_git_call(
      '{0} {1}-- "{2}"'.format(
          cmd, '-i ' if include_staged_files else '', '" "'.join(files)))[0]

@contract
def initRepo(files_root_path : 'str'):
    """Initializes a Git repository. A .gitattributes file is created with the content '* -text'.
    
    :param files_root_path: The root path of the repository.
    """
    cwd = os.getcwd()
    os.chdir(files_root_path)
    gitpylib.repo.init()
    
    os.chdir(cwd)
    
    # Create .gitattributes that disables EOL conversion on Windows:
    gitattrib = os.path.join(files_root_path, '.gitattributes')
    with open(gitattrib, 'w', newline='\n') as f:
        f.write('* -text')
    
    gitCommit(files_root_path, '.gitattributes', 'Created .gitattributes', 'docker')

@contract
def gitCommit(files_root_path : 'str', file_path : 'str', commit_message: 'str', author : 'str'):
    """Commits the specified file to Git repository.
    
    :param files_root_path: The root path of the repository.
    :param file_path: The path of the file to commit.
    :param commit_message: The commit message.
    :param author: The author of the commit.
    :returns: The hash of the change.
    """
    cwd = os.getcwd()
    os.chdir(files_root_path)
    gitpylib.file.stage(file_path)

    try:
        customCommit([file_path], commit_message, author, skip_checks=False, include_staged_files=False)
        latest_hash, err = gitpylib.common.safe_git_call('rev-parse HEAD') # Gets the latest version hash
    except Exception as e:
        exText = str(e)
        if ('nothing added to commit' in exText) or ('no changes added to commit' in exText):
            raise NothingToCommitException()
        raise TimDbException('Commit failed. ' + exText)
    finally:
        os.chdir(cwd)
    return latest_hash.rstrip()

@contract
def gitCommand(files_root_path : 'str', command : 'str'):
    """Executes the specified Git command.
    
    :param files_root_path: The root path of the repository.
    :param command: The command to execute.
    """
    cwd = os.getcwd()
    os.chdir(files_root_path)
    try:
        output, err = gitpylib.common.safe_git_call(command)
    except Exception:
        raise TimDbException('Git call failed')
    finally:
        os.chdir(cwd)
    return output, err
