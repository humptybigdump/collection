'''
@author: Christian Wressnegger
'''

DATE_FORMAT = ("%H:%M %d.%m.%Y", "11:59 31.10.2024")
STR1 = ("Of all the things I've lost, I miss my mind the most",
        "Of all the things...")
STR2 = '''
        Dade?
        Yeah, ma??
        What are you doing?
        I'm taking over a TV network.
        Finish up, honey, and get to sleep.
        '''

CALL = (lambda ex: ex(c=None, a=1, d=4, b='2'))
OUTPUT = '''a = 1\nb = 2\nc = None\nd = 4'''

import os
FILE_LIST = (
    os.getcwd(), "__init__.py\nex00.py\nex00_testdata.py\nexercises.py", ".py")
ARGPARSE = ("test.tar.gz --int 23 -b",
            b"input: test.tar.gz\n--bool True\n--float 0.0\n--int 23")
