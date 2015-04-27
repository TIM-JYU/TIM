import os
import shlex
from contracts import contract, new_contract
import gitpylib.common
import pygit2
from timdb.timdbbase import TimDbException
from datetime import datetime

class NothingToCommitException(Exception):
    pass

new_contract('Commit', pygit2.Commit)

class GitClient:
    def __init__(self, repository):
        self.author = pygit2.Signature('docker', 'docker')
        self.repo = repository

    @classmethod
    def initRepo(cls, path : 'str'):
        repo = pygit2.init_repository(path)
        client = GitClient(repo)
        client.add_custom('.gitattributes', '* -text')
        client.commit('Created .gitattributes', first_commit = True)
        return client

    @classmethod
    def connect(cls, path : 'str'):
        repo_path = os.path.join(path, '.git')
        return GitClient(pygit2.Repository(repo_path))

    @contract
    def get_contents(self, commit_hash : 'str', file_path : 'str'):
        c = self.repo.get(commit_hash)
        if type(c) is not pygit2.Commit:
            raise TimDbException('The requested revision {} was not found. HEAD is {}, c is {}'.format(commit_hash, self.repo.head.target.hex, str(c)))
        if not self.__itemExists(c, file_path):
            raise TimDbException('The requested file {} was not found in the commit {}.'.format(file_path, commit_hash))
        try:
            entry = c.tree[file_path]
        except KeyError:
            raise TimDbException('The requested file {} was not found in the commit {}.'.format(file_path, commit_hash))

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
    def exists(self, path : 'str') -> 'bool':
        index = self.repo.index
        index.read()
        return path in index

    @contract
    def commit(self, message : 'str', author : 'str' = 'docker', first_commit : 'bool' = False) -> 'str':
        signature = self.author if author == 'docker' else pygit2.Signature(author, author)
        index = self.repo.index
        if not first_commit:
            # Check if there actually is anything to commit
            if self.repo.revparse_single('master').tree.diff_to_index(self.repo.index).patch is None:
                raise NothingToCommitException
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
    def __itemExists(self, commit : 'Commit', path : 'str') -> 'bool':
        path_components = path.split('/')
        tree = commit.tree
        for i in range(0, len(path_components)):
            if not path_components[i] in tree:
                return False
            tree_id = tree[path_components[i]].oid
            tree = self.repo[tree_id]
        return True

    @contract
    def __getLatestCommit(self, path : 'str') -> 'Commit|None':
        cur_commit = self.repo[self.repo.head.target]
        last_commit = cur_commit
        file_obj = cur_commit.tree[path]
        while len(cur_commit.parents) > 0:
            cur_commit = cur_commit.parents[0]
            if not self.__itemExists(cur_commit, path):
                #print('__getLastCommit: no exist in {}, choosing {}'.format(cur_commit.oid.hex, last_commit.oid.hex))
                return last_commit

            new_file_obj = cur_commit.tree[path]
            if new_file_obj.oid != file_obj.oid or new_file_obj.filemode != file_obj.filemode:
                #print('__getLastCommit: file id {} differs, choosing {}'.format(cur_commit.oid.hex, last_commit.oid.hex))
                return last_commit

            last_commit = cur_commit
        #print('__getLastCommit: oid {}, exists={}'.format(cur_commit.oid.hex, self.__itemExists(cur_commit, path)))
        return cur_commit if self.__itemExists(cur_commit, path) else None

    @contract
    def getLatestVersion(self, path : 'str') -> 'str|None':
        commit = self.__getLatestCommit(path)
        return commit.hex if commit is not None else None

    @contract
    def getRelativeTimeStr(self, timestamp : 'int') -> 'str':
        delta = datetime.fromtimestamp(timestamp) - datetime.now()
        if delta.days < 0 or delta.seconds < 0:
            return 'in the future'

        if delta.days > 0:
            if delta.days >= 365:
                return '{:.0f} years ago'.format(delta.days / 365)
            if delta.days >= 70:
                return '{:.0f} months ago'.format(delta.days / 30)
            if delta.days >= 7:
                return '{:.0f} weeks ago'.format(delta.days / 7)

        if delta.seconds >= 3600:
            return '{:.0f} hours ago'.format(delta.seconds / 3600)
        if delta.seconds >= 60:
            return '{:.0f} minutes ago'.format(delta.seconds / 60)

        return '{:.0f} seconds ago'.format(delta.seconds)

    @contract
    def __getCommitDetails(self, commit : 'Commit') -> 'dict(str:str)':
        return {
            'hash': commit.hex,
            'timestamp': self.getRelativeTimeStr(commit.commit_time),
            'user': commit.committer.name,
            'message': commit.message }

    @contract
    def getLatestVersionDetails(self, path : 'str') -> 'dict(str:str)|None':
        commit = self.__getLatestCommit(path)
        return self.__getCommitDetails(commit) if commit is not None else None

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

