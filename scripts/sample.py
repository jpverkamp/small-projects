import random, sys

# get the chance
# use exceptions for sanity checks
try:

    if len(sys.argv) != 2: raise Exception()
    chance = float(sys.argv[1])
    if chance < 0 or chance > 1: raise Exception()
except:
    print('''Usage: sample [chance]

Forward [chance] percentage of stdin to stdout
[chance] must be in the range [0,1]''')
    sys.exit(0)

# now just read line by line and output
# based on that random chance
for line in sys.stdin:
    if random.random() < chance:
        print(line[:-1])

