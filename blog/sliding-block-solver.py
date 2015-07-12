import collections
import copy

class Block(object):
    def __init__(self, origin, offsets, group = None):
        self.group = group
        self.origin = origin
        self.offsets = offsets

    def __str__(self):
        '''Return a string of all of the points in this block and its id'''

        return 'Block {group}{pts}'.format(
            group = '({group}) '.format(group = self.group) if self.group else '',
            pts = list(self)
        )

    def __iter__(self):
        '''Return all points in this block (in world coordinates)'''

        for offset in self.offsets:
            yield (
                self.origin[0] + offset[0],
                self.origin[1] + offset[1]
            )

    def neighbors(self):
        '''Generate any points adjacent to this block (not diagonally) but not in it.'''

        yielded = set()
        for offset in self.offsets:
            for xd, yd in ((-1, 0), (1, 0), (0, -1), (0, 1)):
                pt = (
                    self.origin[0] + offset[0] + xd,
                    self.origin[1] + offset[1] + yd
                )
                if not pt in self:
                    yield pt

    def __contains__(self, pt):
        '''Check if a point (in world coordinates) is within this block'''

        return (pt[0] - self.origin[0], pt[1] - self.origin[1]) in self.offsets

class World(object):
    def __init__(self, blocks):
        self.blocks = blocks
        self.history = []

        self.width = 0
        self.height = 0

        for block in self.blocks:
            for tile in block:
                self.width = max(self.width, tile[0])
                self.height = max(self.height, tile[1])

        self.width += 1
        self.height += 1

    @classmethod
    def load(cls, str):
        '''
        Load a world from an ascii grid of 2 character tiles

        -- is an empty space
        XX is a wall
        #  is a tile that doesn't have to be grouped to win
        #* is a tile that has to touch any matching *s to win
        '''

        raw_blocks = collections.defaultdict(set)

        # Load blocks into a general form
        for y, line in enumerate(str.split('\n')):
            for x, item in enumerate(line.split()):

                if item == '--': # Empty space
                    pass
                else: # Pieces
                    raw_blocks[item].add((x, y))

        # Normalize each block
        blocks = []
        for block_id in raw_blocks:
            origin = list(raw_blocks[block_id])[0]
            offsets = []
            for piece in raw_blocks[block_id]:
                offsets.append((
                    piece[0] - origin[0],
                    piece[1] - origin[1]
                ))

            if len(block_id) == 1:
                blocks.append(Block(origin, offsets))
            else:
                blocks.append(Block(origin, offsets, block_id[1]))

        return cls(blocks)

    def __str__(self):
        '''Return the string representation of the world in the same format as load.'''

        str = ''

        generated_ids = {}
        next_id = 0
        ids = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

        for y in range(self.height):
            for x in range(self.width):
                found = False

                if not found:
                    for block in self.blocks:
                        if (x, y) in block:
                            if not block in generated_ids:
                                id = ids[next_id]
                                generated_ids[block] = id
                                next_id += 1

                            str += generated_ids[block]
                            if block.group:
                                str += block.group
                            else:
                                str += ' '

                            found = True
                            break

                if not found:
                    str += '--'
                    found = True

                str += ' '

            str += '\n'

        return str

    def __eq__(self, other):
        '''Compare via stringing both. Inefficient, but it works!'''

        return hash(self) == hash(other)

    def __hash__(self):
        '''Hash is based on the string so swapping identical blocks are identical.'''

        return hash(str(self))

    def __getitem__(self, pt):
        '''Get the block at the specified coordinates (or none if no block matches)'''

        for block in self.blocks:
            if pt in block:
                return block

        return None

    def move(self, block, direction):
        '''
        Try to move a piece in the given direction (N, W, E, W)

        Return the new world if the piece can move.
        '''

        xd, yd = {
            'N': ( 0, -1),
            'S': ( 0,  1),
            'E': ( 1,  0),
            'W': (-1,  0),
        }[direction]

        # Cache the old origin to reset and try to move it
        old_origin = block.origin
        block.origin = (
            block.origin[0] + xd,
            block.origin[1] + yd
        )

        # Check if any point in this block now overlaps anything else
        can_move = True
        for point in block:
            for other_block in self.blocks:
                if block == other_block:
                    continue

                if point in other_block:
                    can_move = False
                    break

            if not can_move:
                break

        # Revert if we cannot move there
        if not can_move:
            block.origin = old_origin
            return self

        # Otherwise, clone, reset the original, and add the move that got us here
        new_world = copy.deepcopy(self)
        block.origin = old_origin
        new_world.history.append((block, direction))

        return new_world

    def solved(self):
        '''A puzzle is True if it's solved.'''

        for block in self.blocks:
            for other_block in self.blocks:
                # Don't try to match with yourself
                if block == other_block:
                    continue

                # Both blocks must have groups
                if not (block.group and other_block.group):
                    continue

                # Don't match if the groups don't match
                if block.group != other_block.group:
                    continue

                # Otherwise at least one neighbor of one block must be in the other block
                neighbors = False
                for point in block.neighbors():
                    if point in other_block:
                        neighbors = True
                        break
                if neighbors:
                    continue

                # If we made it through all of these checks, we have a group not matching
                return False

        # If we made it out of the loop, yay! We win.
        return True

def solve(world):

    states = collections.defaultdict(set)
    queue = set([world])

    while queue:
        # Don't recalculate states
        world = queue.pop()
        if world in states:
            continue

        print('queue:', len(queue))
        print('# of states:', len(states))
        print(world)

        # Otherwise, try to move every single point in every single direction
        # Whee!
        for block in world.blocks:
            for d in 'NSEW':
                new_world = world.move(block, d)

                if new_world == world:
                    continue

                if new_world.solved():
                    return new_world

                if new_world in states or new_world in queue:
                    continue

                states[world].add(new_world)
                queue.add(new_world)

    raise Exception('No solution found')

w = World.load('''
XX XX XX XX XX XX XX XX
XX 1  1  2  -- -- 3  XX
XX 4y 4y 2  -- -- 3  XX
XX XX XX XX 5  5  6  XX
-- -- -- XX XX -- 6  XX
-- -- -- -- XX 7y 7y XX
-- -- -- -- XX XX XX XX
''')

final = solve(w)

print('===== ===== ===== ===== =====\n')

print(final.history)

print(w)
for (block, d) in final.history:
    w = w.move(block, d)
    print(w)
