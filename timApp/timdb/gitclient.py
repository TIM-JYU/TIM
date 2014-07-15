import os
from contracts import contract
import gitpylib.repo
import gitpylib.file
import gitpylib.common
import gitpylib.sync
from timdb.timdbbase import TimDbException

#TODO: This should possibly be a class.

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
    """
    cwd = os.getcwd()
    os.chdir(files_root_path)
    gitpylib.file.stage(file_path)
    # TODO: Set author for the commit (need to call safe_git_call).
    try:
        gitpylib.sync.commit([file_path], commit_message, skip_checks=False, include_staged_files=False)
        latest_hash, err = gitpylib.common.safe_git_call('rev-parse HEAD') # Gets the latest version hash
    except Exception as e:
        if 'nothing added to commit' in str(e):
            return
        raise TimDbException('Commit failed. ' + str(e))
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
    output, err = gitpylib.common.safe_git_call(command)
    os.chdir(cwd)
    return output, err
