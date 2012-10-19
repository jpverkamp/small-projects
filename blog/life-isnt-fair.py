import random

class Coin(object):
	def __init__(self, r):
		self.r = r
		self.f = 0

	def flip(self):
		self.f += 1
		return random.random() < self.r

if __name__ == '__main__':
	for tries in xrange(5):
		r = random.random()
		print 'Generating a coin with r = %s' % r

		c = Coin(r)

		h = 0
		t = 0
		for i in xrange(10000):
			while True:
				a = c.flip()
				b = c.flip()

				if a != b:
					if a:
						h += 1
					else:
						t += 1
					break

		print '''
=== Results ===
heads: %d (%.2f%%)
tails: %d (%.2f%%)
flips: %d
''' % (h, 100.0 * h / (h + t), t, 100.0 * t / (h + t), c.f)
