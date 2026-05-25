#!/usr/bin/env python
import subprocess
import sys
import os

# Change to the repository root
os.chdir('E:\\tim')

# Run pytest with the specific tests
result = subprocess.run([
    sys.executable, '-m', 'pytest',
    'timApp/tests/server/test_editing.py::EditTest::test_edit_attribute',
    'timApp/tests/server/test_editing.py::EditTest::test_area_edit_requires_doc_edit_for_structural_changes',
    '-v'
], capture_output=False, text=True)

sys.exit(result.returncode)
