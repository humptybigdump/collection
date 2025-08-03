#!/usr/bin/env python3

'''
@author: Christian Wressnegger
'''

try:
    # For evaluating the exercises we'll provide a similar but
    # different configuration that contains alternative input
    # values than those provided in the script that was handed
    # out (nothing mean though). Develop your solution robust
    # enough to work with various kinds and variations of input.
    import ex00_testdata_lecturer as testdata  # @UnresolvedImport @UnusedImport

except:
    import ex00_testdata as testdata  # @UnusedImport

from base64 import b64encode
import os
import subprocess
import sys
import types
import unittest


USAGE = b'''usage: exercises.py [-h] [-b] [-f FLOAT] [-i INT] FILE

positional arguments:
  FILE                  The input positional parameter.

options:
  -h, --help            show this help message and exit
  -b, --bool            An optional boolean flag (Default: False).
  -f FLOAT, --float FLOAT
                        An optional parameter of type float (Default: 0.0).
  -i INT, --int INT     An optional parameter of type int (Default: 0).'''


try:
    from intellisec.exercises import Exercise00
except ImportError:
    pass


unittest.TestLoader.sortTestMethodsUsing = None
PYTHON = "python3"
PYERROR = "For running your solution we call '{}'.\nThe name might be different for your installation (e.g. on Windows)\n"


class Ex00(unittest.TestCase):

    SCORE = 0

    def test_00_packages(self):
        self.assertTrue("Exercise00" in globals())
        Ex00.SCORE += 1

    def test_01_static_field(self):
        try:
            print("[I] Name: " + Exercise00.STUDENT_NAME)
            Ex00.SCORE += 1

        except AttributeError:
            self.assertFalse(True, "No name specified")

    def test_02_static_method(self):
        self.assertEqual(
            Exercise00.deadline(testdata.DATE_FORMAT[0]), testdata.DATE_FORMAT[1])
        Ex00.SCORE += 1

    def test_03_property(self):
        ex = Exercise00(testdata.STR1[0])
        try:
            ex.txt = "test"
            self.assertFalse(True, "not a property")

        except AttributeError as e:
            self.assertIn(
                "can't set attribute", str(e), "no setter allowed")

        s = ex.txt
        Ex00.SCORE += 1

        self.assertEqual(s, testdata.STR1[1], "wrong output")
        Ex00.SCORE += 1

    def test_04_format_strings(self):
        def get_format(ex, mode):
            fmt = ex.format(mode)
            if '%' in fmt:
                self.assertFalse(True, "Python-2 format string")

            return fmt

        ex = Exercise00()

        fmt = get_format(ex, "order")
        s = fmt.format('third', 'second', 'first')
        self.assertEqual(s, "first - second - third")
        Ex00.SCORE += 1

        fmt = get_format(ex, "dict")

        d = {'x': 41.123, 'y': 71.091}
        s = fmt.format(**d)

        self.assertEqual(s, "x, y = (41.1, 71.0910)")
        Ex00.SCORE += 1

    def test_05_generators(self):
        ex = Exercise00()
        g = ex.listfiles(testdata.FILE_LIST[0])
        self.assertTrue(isinstance(g, types.GeneratorType), "not a generator")
        Ex00.SCORE += 1

        g = ex.listfiles(testdata.FILE_LIST[0], testdata.FILE_LIST[2])
        s = '\n'.join(sorted(g))

        self.assertEqual(
            s, testdata.FILE_LIST[1], "wrong list of files")
        Ex00.SCORE += 1

    def test_06_functionparams(self):
        ex = Exercise00()
        s = testdata.CALL(ex)

        Ex00.SCORE += 1
        self.assertEqual(s, testdata.OUTPUT, "wrong output")
        Ex00.SCORE += 1

    def test_07_base64(self):
        ex = Exercise00(testdata.STR2)
        s = str(ex)
        self.assertEqual(
            s, b64encode(testdata.STR2.encode()).decode(), "wrong output")
        Ex00.SCORE += 8

    def test_08_argparse(self):
        def call(params):
            my_dir = os.path.dirname(os.path.abspath(__file__))
            script = os.path.join(my_dir, "intellisec", "exercises.py")
            cmd = f'{PYTHON} "{script}" {params}'

            p = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
            out, _ = p.communicate()

            if p.returncode not in [0, 42]:
                sys.stderr.write(PYERROR.format(PYTHON))

            return out, p.returncode

        out, _ = call("-h")

        self.assertEqual(
            out.strip().replace(b'\r', b''), USAGE.replace(b'\r', b''), "wrong usage information")
        Ex00.SCORE += 3

        out, ret = call(testdata.ARGPARSE[0])

        self.assertEqual(ret, 42, "wrong return code")
        Ex00.SCORE += 1

        self.assertEqual(out.strip(), testdata.ARGPARSE[1], "wrong output")
        Ex00.SCORE += 1

    def test_XX(self):
        print(f"[*] Total score for Exercise 00: {Ex00.SCORE}/24")


if __name__ == "__main__":
    unittest.main()
