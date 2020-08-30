import os
import re
import subprocess
import tempfile
import unittest
from collections import OrderedDict
from pprint import PrettyPrinter

import xmlrunner


class TestsContainer(unittest.TestCase):
    longMessage = True
    expected = 48
    ran = 0
    logs = os.getcwd()

    def test_all_tests_ran(self):
        self.assertEqual(self.expected, self.ran)

    def test_logs_have_no_errors(self):
        def the_pred(de):
            if de.name.endswith('.ESMF_LogFile'):
                return True
            else:
                return False

        to_find = ['ERROR']
        problems = []
        for de in walk_files(self.logs, the_pred):
            with open(de.path, 'r') as f:
                lines = f.readlines()
                for line_number, line in enumerate(lines, start=1):
                    line = line.strip()
                    for tf in to_find:
                        if tf in line:
                            problems.append((de, line, line_number))

        msg = ['\n']
        for tup in problems:
            de, line, line_number = tup
            msg.append(de.name + ':' + str(line_number) + ': ' + line)
        msg = '\n'.join(msg)

        self.assertEqual(len(problems), 0, msg=msg)


def make_test_function(description, pass_fail):
    def test(self):
        self.assertEqual(pass_fail, "PASS", description)
    return test


def walk_files(path, pred):
    for de in os.scandir(path):
        if de.is_dir():
            for de2 in walk_files(de.path, pred):
                yield de2
        else:
            if pred(de):
                yield de


def make_testsmap_nuopc_protos(outfile):
    testsmap = OrderedDict()
    ran = 0
    with open(outfile, 'r') as f:
        parse = False
        for line in f:
            line = line.strip()
            if '== TEST SUMMARY STOP ==' in line:
                break
            elif '== TEST SUMMARY START ==' in line:
                parse = True
            elif parse:
                result = re.search('(?P<pass_fail>.*): (?P<name>.*)$', line)
                testsmap[result['name']] = result['pass_fail']
                ran += 1
    testsmap['ran'] = ran
    return testsmap


if __name__ == '__main__':
    xmlout = os.path.join(os.environ['ESMF_ARTIFACTS'], 'meta_test')
    outfile = os.path.join(os.environ['ESMF_ARTIFACTS'], 'testProtos.out')

    testsmap = make_testsmap_nuopc_protos(outfile)

    ran = testsmap.pop('ran')
    TestsContainer.ran = ran

    for name, pass_fail in testsmap.items():
        test_func = make_test_function(name, pass_fail)
        setattr(TestsContainer, 'test_{0}'.format(name), test_func)

    unittest.main(testRunner=xmlrunner.XMLTestRunner(output=xmlout))
