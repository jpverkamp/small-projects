# Key function to sort by an arbitrary alphabet
def make_gorellian(alphabet):
	# Convert a string into a list of numbers
	def to_offset_list(string):
		return [alphabet.index(c) for c in string]
	return to_offset_list

# Make sure everything works as expected
assert list(sorted(
	['ANTLER', 'ANY', 'COW', 'HILL', 'HOW', 'HOWEVER', 'WHATEVER', 'ZONE'],
	key = make_gorellian('UVWXYZNOPQRSTHIJKLMABCDEFG')
)) == ['WHATEVER', 'ZONE', 'HOW', 'HOWEVER', 'HILL', 'ANY', 'ANTLER', 'COW']
