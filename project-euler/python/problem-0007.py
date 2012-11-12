# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
# we can see that the 6th prime is 13.

# What is the 10 001st prime number?
def problem_0007():
	'''Solve Project Euler #7
	
	By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
	we can see that the 6th prime is 13.

	What is the 10 001st prime number?'''
	
	return nth_prime(10001)

def nth_prime(n):
    '''Return the nth prime number.'''
    
    p = 1
    for i in xrange(n):
        p = next_prime(p)
    
    return p
    

def next_prime(n):
    '''Return the next prime after a given number.'''
    
    n += 1
    while not is_prime(n):
        n += 1
        
    return n

def is_prime(n):
    '''Test if a number is prime by simple trial division.'''
    
    if n == 2: return True
    elif n % 2 == 0: return False
    for i in xrange(3, 1 + int(n ** 0.5), 2):
        if n % i == 0: return False
    
    return True
    
def time(code):
	import time
	t = time.time()
	res = eval(code)
	print '%0.2f seconds' % (time.time() - t)
	return res

