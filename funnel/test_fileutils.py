import unittest
from shutil import rmtree

from fileutils import *


class QueueTest(unittest.TestCase):
    def setUp(self):
        self.dir = os.path.join(os.getcwd(), 'test_fileutils')
        if os.path.exists(self.dir):
            rmtree(self.dir)

        os.makedirs(self.dir)
        self.files = [self.mkfile() for i in range(0, 5)]
        self.dirs = [self.mkdir() for i in range(0, 3)]
        self.links = [self.mklink(name) for name in self.files[:2]]
        self.dlinks = [self.mklink(name) for name in self.dirs[:1]]

    def tearDown(self):
        if os.path.exists(self.dir):
            rmtree(self.dir)

    def mkfile(self):
        abs_name, rel_name = get_random_filenames(self.dir, prefix='file_')
        with open(abs_name, mode='xt') as f:
            f.write('test')
        return rel_name

    def mkdir(self):
        abs_name, rel_name = get_random_filenames(self.dir, prefix='dir_')
        os.makedirs(abs_name)
        return rel_name

    def mklink(self, target: str):
        abs_name, rel_name = get_random_filenames(self.dir, prefix='link_')
        os.symlink(target, abs_name)
        return rel_name

    def testRandomFilename(self):
        for i in range(10):
            random_abs, random_rel = get_random_filenames(self.dir)
            self.assertEqual(random_abs, os.path.join(self.dir, random_rel))
            self.assertFalse(os.path.isfile(random_abs))
            with open(random_abs, 'w') as f:
                pass
            self.assertTrue(os.path.isfile(random_abs))

    def test_listdirs(self):
        test_dirs = listdirs(self.dir)
        self.assertSetEqual(set(test_dirs), set(self.dirs + self.dlinks))

    def test_listfiles(self):
        test_files = listfiles(self.dir)
        self.assertSetEqual(set(test_files), set(self.files + self.links))

    def test_listnormalfiles(self):
        test_files = listnormalfiles(self.dir)
        self.assertSetEqual(set(test_files), set(self.files))

