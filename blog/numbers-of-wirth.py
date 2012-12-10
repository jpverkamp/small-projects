# Develop a program that generates in ascending order the least 100 numbers of the
# set M, where M is defined as follows:
# 
# a) The number 1 is in M.
# 
# b) If x is in M, then y = 2 * x + 1 and z = 3 * x + 1 are also in M.
# 
# c) No other numbers are in M.
# 
# -- Systematic Programming: An Introduction, exercise 15.12

# --- ORIGINAL VERSION ---

def is_wirth(n):
	'''Recursively determine if a number is a wirth number'''

	if n < 0 or n != int(n):
		return False
	elif n == 1:
		return True
	else:
		return is_wirth((n - 1) / 2.0) or is_wirth((n - 1) / 3.0)
		
def n_wirth(n):
	'''List the first n wirth numbers'''
	
	ls = []
	i = 1
	while len(ls) < n:
		if is_wirth(i):
			ls.append(i)
		i += 1
		
	return ls
	
# --- GENERATOR VERSION ---

def merge_generators(g1, g2):
	'''Merge two numeric generators in increasing order.'''
	
	n1 = g1.next()
	n2 = g2.next()
	while True:
		if n1 < n2:
			yield n1
			n1 = g1.next()
		elif n1 > n2:
			yield n2
			n2 = g2.next()
		else:
			yield n1
			n1 = g1.next()
			n2 = g2.next()

def wirth_gen(n):
	'''A generator for wirth numbers.'''
	yield n
	for i in merge_generators(wirth_gen(2 * n + 1), wirth_gen(3 * n + 1)):
		yield i

def take(gen, n):
	'''Take the first n items from a generator.'''
	ls = []
	while len(ls) < n:
		ls.append(gen.next())
	return ls
	
def gen_n_wirth(n):
	'''List the first n wirth numbers using generators'''
	return take(wirth_gen(1), n)

# --- SIEVED VERSION ---

def sieve_n_wirth(n, size):
	'''Generate the first n wirth numbers using a sieve.'''
	
	sieve = [False for i in xrange(size)]
	sieve[1] = True
	ls = []
	for i in xrange(size):
		if sieve[i]:
			ls.append(i)
			if 2 * i + 1 < size: sieve[2 * i + 1] = True
			if 3 * i + 1 < size: sieve[3 * i + 1] = True
			if len(ls) == n:
				break
	return ls
	
# --- HEAP ---

from heapq import heappop, heappush 

def heap_n_wirth(n):
	'''Generate the first n wirth numbers using a heap.'''
	
	ls = []
	heap = []
	heappush(heap, 1)
	while len(ls) < n:
		i = heappop(heap)
		if not i in ls:
			ls.append(i)
			heappush(heap, 2 * i + 1)
			heappush(heap, 3 * i + 1)
	return ls
