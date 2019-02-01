# -*- coding: utf-8 -*-

Created on Tue Sep 25 20:47:32 2018

@author: evinb

N = 20
A = list(range(1,21))
print (A)
target = 3
def linearSearch(A, target, F):
    for x in range(0, len(F)):
        if (F[x]==target):
          return x
        return False