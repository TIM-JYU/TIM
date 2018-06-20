import os

from yaml.constructor import ConstructorError

from timApp.document.yamlblock import YamlBlock
from timApp.tests.db.timdbtest import TimDbTest


class SafeYamlTest(TimDbTest):
    def test_safe_yaml(self):
        test_path = f'{self.test_files_path}/unsafe'
        with self.assertRaises(ConstructorError):
            YamlBlock.from_markdown(f"""
asd: !!python/object/apply:os.system ['touch {test_path}']
            """)
        self.assertFalse(os.path.exists(test_path))
