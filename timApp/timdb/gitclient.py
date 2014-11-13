import os
import shlex
from contracts import contract
import gitpylib.common
import pygit2
from timdb.timdbbase import TimDbException


class NothingToCommitException(Exception):
    pass


class GitClient:
    def __init__(self, repository):
        self.author = pygit2.Signature('docker', 'docker')
        self.repo = repository

    @classmethod
    def initRepo(cls, path):
        repo = pygit2.init_repository(path)
        client = GitClient(repo)
        client.add_custom('.gitattributes', '* -text')
        client.commit('Created .gitattributes', first_commit = True)
        return client

    @classmethod
    def connect(cls, path):
        repo_path = os.path.join(path, '.git')
        return GitClient(pygit2.Repository(repo_path))

    @contract
    def get_contents(self, commit_hash : 'str', file_path : 'str'):
        c = self.repo.get(commit_hash)
        if c is not pygit2.Commit:
            raise TimDbException('The requested revision was not found.')
        if not c.contains(file_path):
            raise TimDbException('The requested file was not found in the commit.')
        entry = c.tree[file_path]
        blob = self.repo[entry.oid]
        return blob.data

    @contract
    def add(self, path : 'str'):
        index = self.repo.index
        index.read()
        index.add(path)
        index.write()

    @contract
    def add_custom(self, path : 'str', content : 'str'):
        with open(os.path.join(self.repo.workdir, path), 'w', newline='\n') as f:
            f.write(content)
        self.add(path)

    @contract
    def rm(self, path : 'str'):
        index = self.repo.index
        index.read()
        index.remove(path)
        index.write()

    @contract
    def commit(self, message : 'str', author = 'docker', first_commit : 'bool' = False) -> 'str':
        signature = self.author if author == 'docker' else pygit2.Signature(author, author)
        index = self.repo.index
        tree = index.write_tree()
        parent = [] if first_commit else [self.repo.head.target]
        oid = self.repo.create_commit(
            'refs/heads/master',
            signature, signature,
            message,
            tree, parent
        )
        index.write()
        return oid.hex

    @contract
    def command(self, command: 'str') -> 'tuple(str, str)':
        """Executes the specified Git command.

        :param files_root_path: The root path of the repository.
        :param command: The command to execute.
        """
        cwd = os.getcwd()
        os.chdir(self.repo.workdir)
        try:
            output, err = gitpylib.common.safe_git_call(command)
        except Exception:
            raise TimDbException('Git call failed')
        finally:
            os.chdir(cwd)
        return output, err

