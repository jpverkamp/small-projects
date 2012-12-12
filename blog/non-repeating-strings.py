# --- BRUTE FORCE SOLUTION ---

def combinations(N, A):
	'''Generate all strings from A of length N.'''
	if N == 0:
		yield ''
	else:
		for c in A:
			for s in combinations(N - 1, A):
				yield c + s
				
assert \
	list(combinations(3, 'ab')) == \
	['aaa', 'aab', 'aba', 'abb', 'baa', 'bab', 'bba', 'bbb']
				
def has_repeat(s):
	'''Check if a string has a non-empty repeating substring.'''
	
	N = len(s)
	for i in xrange(1, N):
		for j in xrange(1, min(i, N - i) + 1):
			if s[i-j:i] == s[i:i+j]:
				return True

	return False
	
assert has_repeat('abcbc') == True
assert has_repeat('ababc') == True
assert has_repeat('abbca') == True
assert has_repeat('abcba') == False
assert has_repeat('dabcabcd') == True

def non_repeating_direct(N, A):
	'''
	Generate all strings of length N from the alphabet A
	such that no adjacent substrings are repeated.
	'''

	return [s for s in combinations(N, A) if not has_repeat(s)]
	
assert \
	non_repeating_direct(5, 'abc') == \
	['abaca', 'abacb', 'abcab', 'abcac', 'abcba',
	 'acaba', 'acabc', 'acbab', 'acbac', 'acbca',
	 'babca', 'babcb', 'bacab', 'bacba', 'bacbc',
	 'bcaba', 'bcabc', 'bcacb', 'bcbab', 'bcbac',
	 'cabac', 'cabca', 'cabcb', 'cacba', 'cacbc',
	 'cbabc', 'cbaca', 'cbacb', 'cbcab', 'cbcac']
