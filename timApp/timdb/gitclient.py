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
        if type(c) is not pygit2.Commit:
            raise TimDbException('The requested revision {} was not found. HEAD is {}, c is {}'.format(commit_hash, self.repo.head.target.hex, str(c)))
        if not self.__itemExists(c, file_path):
            raise TimDbException('The requested file {} was not found in the commit {}.'.format(file_path, commit_hash))
        entry = c.tree[file_path]
        blob = self.repo[entry.oid]
        return blob.data.decode()

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
    def commit(self, message : 'str', author : 'str' = 'docker', first_commit : 'bool' = False) -> 'str':
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

    def __itemExists(self, commit, path):
        path_components = path.split('/')
        tree = commit.tree
        for i in range(0, len(path_components)):
            if not path_components[i] in tree:
                return False
            tree_id = tree[path_components[i]].oid
            tree = self.repo[tree_id]
        return True

    @contract
    def getLatestVersion(self, path : 'str') -> 'str|None':
        cur_commit = self.repo[self.repo.head.target]
        last_commit = cur_commit
        file_id = cur_commit.tree[path].oid
        while len(cur_commit.parents) > 0:
            cur_commit = cur_commit.parents[0]
            if not self.__itemExists(cur_commit, path):
                return last_commit.hex

            new_file_id = cur_commit.tree[path].oid
            if new_file_id.raw != file_id.raw:
                return last_commit.hex

            last_commit = cur_commit
        return cur_commit.hex

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

