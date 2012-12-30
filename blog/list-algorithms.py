#!/usr/bin/env python

def intersection_loops(ls1, ls2):
	'''Calculate the intersection of two lists using nested loops.'''
	
	result = []
	
	for e in ls1:
		if e in ls2:
			result.append(e)
			
	return result
	
def union_loops(ls1, ls2):
	'''Calculate the union of two lists using nested loops.'''

	result = []
	
	for e in ls1:
		if not e in ls2:
			result.append(e)
			
	for e in ls2:
		result.append(e)
		
	return result
	
def difference_loops(ls1, ls2):
	'''Calculate the difference of two lists using nested loops.'''
	
	result = []
	
	for e in ls1:
		if not e in ls2:
			result.append(e)
			
	return result
	
def intersection_sort(ls1, ls2):
	'''Calculate the intersection of two lists by sorting them first.'''
	
	ls1 = sorted(ls1)
	ls2 = sorted(ls2)
	result = []
	
	i, j = 0, 0
	while True:
		if i >= len(ls1):
			break
		elif j >= len(ls2):
			break
		elif ls1[i] < ls2[j]:
			i += 1
		elif ls1[i] > ls2[j]:
			j += 1
		else: # ==
			result.append(ls1[i])
			i += 1
			j += 1
			
	return result
	
def union_sort(ls1, ls2):
	'''Calculate the union of two lists by sorting them first.'''
	
	ls1 = sorted(ls1)
	ls2 = sorted(ls2)
	result = []
	
	i, j = 0, 0
	while True:
		if i >= len(ls1):
			result += ls2[j:]
			break
		elif j >= len(ls2):
			result += ls1[i:]
			break
		elif ls1[i] < ls2[j]:
			result.append(ls1[i])
			i += 1
		elif ls1[i] > ls2[j]:
			result.append(ls2[j])
			j += 1
		else: # ==
			result.append(ls2[j])
			i += 1
			j += 1
			
	return result

def difference_sort(ls1, ls2):
	'''Calculate the difference of two lists by sorting them first.'''
	
	ls1 = sorted(ls1)
	ls2 = sorted(ls2)
	result = []
	
	i, j = 0, 0
	while True:
		if i >= len(ls1):
			break
		elif j >= len(ls2):
			result += ls1[i:]
			break
		elif ls1[i] < ls2[j]:
			result.append(ls1[i])
			i += 1
		elif ls1[i] > ls2[j]:
			j += 1
		else: # ==
			i += 1
			j += 1
			
	return result
	
def intersection_hash(ls1, ls2):
	'''Calculate the intersection of two lists using a hash.'''
	
	ls2_set = set(ls2)
	result = []
	
	for e in ls1:
		if e in ls2_set:
			result.append(e)
			
	return result
	
def union_hash(ls1, ls2):
	'''Calculate the union of two lists using a hash.'''

	ls2_set = set(ls2)
	result = []
	
	for e in ls1:
		if not e in ls2_set:
			result.append(e)
			
	for e in ls2:
		result.append(e)
		
	return result
	
def difference_hash(ls1, ls2):
	'''Calculate the difference of two lists using a hash.'''
	
	ls2_set = set(ls2)
	result = []
	
	for e in ls1:
		if not e in ls2_set:
			result.append(e)
			
	return result
