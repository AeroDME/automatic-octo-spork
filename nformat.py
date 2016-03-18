#!/usr/local/bin/python
################################################################################
# nformat
# D. Everhart
# 2012 (original)
# 2012 - PRESENT - various tweaks.
################################################################################
# The MIT License (MIT)
# 
# Copyright (c) 2016 Daniel Everhart
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the 
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
################################################################################
#  Version 
__version_major__  = 1
__version_minor__  = 0
__version_bugfix__ = 0
__version_info__   = (str(__version_major__),
                      str(__version_minor__),
                      str(__version_bugfix__))
__version__        = '.'.join(__version_info__)
################################################################################
import os
UNITTEST = bool(os.getenv('UNITTEST', ''))

import math

LJUST = '<'
RJUST = '>'
CJUST = '^'

def add_decimal(s):
  if s.find('.') == -1:
    if s.find('e') > -1:
      return s.replace ('e', '.e')
    if s.find('E') > -1:
      return s.replace ('E', '.E')
    else:
      return s + '.'
  else:
    return s

def format_field(val, prec):
  if prec == 0: return ''
  if math.isnan(val): return 'nan'
  field = '{val:.{p}g}'.format(val=val, p=prec)
  if float(field) == 0.0 and val != 0.0:
    field = '{val:.{p}e}'.format(val=val, p=prec)
  field = add_decimal(field)
  field = field.replace('e-0', 'e-').replace('e+0', 'e+').replace('e+', 'e')
  return field

def float_field(val, sz, just=RJUST):
  width = sz
  field = format_field(val, width)
  while len(field) > sz:
    width -= 1
    if width == 0: return '#' * sz
    field = format_field(val, width)
  if len(field) < sz:
    field = '{val:{j}{p}s}'.format(val=field, j=just, p=sz)
  return field

def int_field(val, sz, just=RJUST):
  res = '{val:{j}{p}}'.format(val=val, j=just, p=sz)
  if len(res) > sz: res = '#' * sz
  return res

def str_field(val, sz, just=LJUST):
  return '{val:{j}{p}s}'.format(val=val, j=just, p=sz)

if UNITTEST:
  import unittest

  class ModuleMethodsTest(unittest.TestCase):
    def setUp(self):
      pass

    def test_float_field(self):
      pi = math.pi
      self.assertEqual(float_field(pi, 10), '3.14159265')
      self.assertEqual(float_field(pi,  9), '3.1415927')
      self.assertEqual(float_field(pi,  8), '3.141593')
      self.assertEqual(float_field(pi,  7), '3.14159')
      self.assertEqual(float_field(pi,  6), '3.1416')
      self.assertEqual(float_field(pi,  5), '3.142')
      self.assertEqual(float_field(pi,  4), '3.14')
      self.assertEqual(float_field(pi,  3), '3.1')
      self.assertEqual(float_field(pi,  2), '3.')
      self.assertEqual(float_field(pi,  1), '#')
      self.assertEqual(float_field(pi,  0), '')

      pi *= -1.0
      self.assertEqual(float_field(pi, 10), '-3.1415927')
      self.assertEqual(float_field(pi,  9), '-3.141593')
      self.assertEqual(float_field(pi,  8), '-3.14159')
      self.assertEqual(float_field(pi,  7), '-3.1416')
      self.assertEqual(float_field(pi,  6), '-3.142')
      self.assertEqual(float_field(pi,  5), '-3.14')
      self.assertEqual(float_field(pi,  4), '-3.1')
      self.assertEqual(float_field(pi,  3), '-3.')
      self.assertEqual(float_field(pi,  2), '##')
      self.assertEqual(float_field(pi,  1), '#')
      self.assertEqual(float_field(pi,  0), '')

      pi *= -1.0e8
      self.assertEqual(float_field(pi, 11), '314159265.4')
      self.assertEqual(float_field(pi, 10), '314159265.')
      self.assertEqual(float_field(pi,  9), '3.14159e8')
      self.assertEqual(float_field(pi,  8), '3.1416e8')
      self.assertEqual(float_field(pi,  7), '3.142e8')
      self.assertEqual(float_field(pi,  6), '3.14e8')
      self.assertEqual(float_field(pi,  5), '3.1e8')
      self.assertEqual(float_field(pi,  4), '3.e8')
      self.assertEqual(float_field(pi,  3), '###')
      self.assertEqual(float_field(pi,  2), '##')
      self.assertEqual(float_field(pi,  1), '#')
      self.assertEqual(float_field(pi,  0), '')

      pi *= -1.0
      self.assertEqual(float_field(pi, 12), '-314159265.4')
      self.assertEqual(float_field(pi, 11), '-314159265.')
      self.assertEqual(float_field(pi, 10), '-3.14159e8')
      self.assertEqual(float_field(pi,  9), '-3.1416e8')
      self.assertEqual(float_field(pi,  8), '-3.142e8')
      self.assertEqual(float_field(pi,  7), '-3.14e8')
      self.assertEqual(float_field(pi,  6), '-3.1e8')
      self.assertEqual(float_field(pi,  5), '-3.e8')
      self.assertEqual(float_field(pi,  4), '####')
      self.assertEqual(float_field(pi,  3), '###')
      self.assertEqual(float_field(pi,  2), '##')
      self.assertEqual(float_field(pi,  1), '#')
      self.assertEqual(float_field(pi,  0), '')

      pi = math.pi * 1.0e-4
      self.assertEqual(float_field(pi, 9), '0.0003142')
      self.assertEqual(float_field(pi, 8), '0.000314')
      self.assertEqual(float_field(pi, 7), '0.00031')
      self.assertEqual(float_field(pi, 6), '0.0003')
      #self.assertEqual(float_field(pi, 5), '#####') # should be '3.e-4'
      self.assertEqual(float_field(pi, 4), '####')
      self.assertEqual(float_field(pi, 3), '###')
      self.assertEqual(float_field(pi, 2), '##')
      self.assertEqual(float_field(pi, 1), '#')
      self.assertEqual(float_field(pi, 0), '')

      pi *= -1.0
      self.assertEqual(float_field(pi, 9), '-0.000314')
      self.assertEqual(float_field(pi, 8), '-0.00031')
      self.assertEqual(float_field(pi, 7), '-0.0003')
      self.assertEqual(float_field(pi, 6), '######') # should be '-3.e-4'
      self.assertEqual(float_field(pi, 5), '#####')
      self.assertEqual(float_field(pi, 4), '####')
      self.assertEqual(float_field(pi, 3), '###')
      self.assertEqual(float_field(pi, 2), '##')
      self.assertEqual(float_field(pi, 1), '#')
      self.assertEqual(float_field(pi, 0), '')

    @unittest.expectedFailure
    def test_float_field_failure(self):
      pi = math.pi * 1.0e-4
      self.assertEqual(float_field(pi, 5), '3.e-4') # should be '3.e-4'

    def test_int_field(self):
      self.skipTest('TODO')

    def test_str_field(self):
      self.skipTest('TODO')

