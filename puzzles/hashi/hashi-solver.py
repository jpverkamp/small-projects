import collections
import copy
import random

class Hashi(object):
    '''
    Store a Hashi puzzle

    self.width: the width of the puzzle
    self.height: the height of the puzzle
    self.nodes: a map of tiles in the puzzle (size or None)
    self.capacity: the edges necessary to fill each node (has an entry for each in self.nodes)
    self.edges: a map of edges in the puzzle (|║-= or None)
    '''

    def __init__(self, fin):
        '''Read a Hashi puzzle from the given file-like object'''

        self.nodes = collections.defaultdict(lambda : None)
        self.capacity = collections.defaultdict(lambda : 0)
        self.edges = collections.defaultdict(lambda : None)

        for row, line in enumerate(fin):
            for col, char in enumerate(line.strip()):
                if char != '.':
                    self.nodes[(row, col)] = self.capacity[(row, col)] = int(char)

        self.height = row
        self.width = col

    def __str__(self):
        '''Print a string representation of a puzzle.'''

        s = ''

        for row in range(self.height + 1):
            for col in range(self.width + 1):
                s += str(self.nodes[(row, col)] or self.edges[(row, col)] or ' ')
            s += '\n'

        return s

    def add_edge(self, src, dst):
        '''Try to add an edge between two nodes. Return if successful.'''

        # We cannot connect a node to itself
        if src == dst:
            return False

        # The nodes aren't on either the same row or column
        if src[0] == dst[0]:
            changing_index = 1
            acceptable_edge = '-'
            upgraded_edge = '='

        elif src[1] == dst[1]:
            changing_index = 0
            acceptable_edge = '|'
            upgraded_edge = '║'

        else:
            return False

        # The nodes don't have any capacity left (or aren't actually nodes)
        if not self.capacity[src] or not self.capacity[dst]:
            return False

        # Find the bounds on the axis that changes
        lo = min(src[changing_index], dst[changing_index])
        hi = max(src[changing_index], dst[changing_index])

        # Make sure that we can go that direction
        # And that there are no edges in the way
        for i in range(lo + 1, hi):
            pt = list(src)
            pt[changing_index] = i
            pt = tuple(pt)

            #print('testing', pt)
            #print('edge currently is', self.edges[pt])

            if self.nodes[pt]:
                #print('FAILED ON NODE')
                return False

            if not self.edges[pt] or self.edges[pt] == acceptable_edge:
                continue

            #print('FAILED ON BAD EDGE')
            return False

        # We've validated the edge is possible, add it
        for i in range(lo + 1, hi):
            pt = list(src)
            pt[changing_index] = i
            pt = tuple(pt)

            if self.edges[pt] == acceptable_edge:
                self.edges[pt] = upgraded_edge
            else:
                self.edges[pt] = acceptable_edge

        # Remove one capacity from both nodes
        self.capacity[src] -= 1
        self.capacity[dst] -= 1

        return True

    def solved(self):
        '''Test if the current puzzle is solved (no node needs more edges.)'''

        return not any(self.capacity)

    @staticmethod
    def solve(puzzle, edge_iter):
        '''
        Solve using backtracking

        @param puzzle the Hashi object to solve
        @param edge_iter an iterator of pairs of nodes to drive the solver
        @return a solved puzzle or False if it's not solveable
        '''

        queue = [(puzzle, edge_iter(puzzle))]
        seen = set()
        tries = 0

        # DEBUG
        indexes = collections.defaultdict(lambda : 0)

        while queue:
            (current, current_edge_iter) = queue.pop()
            seen.add(current)
            tries += 1

            # DEBUG
            indexes[current_edge_iter] += 1

            # DEBUG
            print(len(queue), tries, indexes[current_edge_iter])
            print(current)
            #print(queue[:2])
            #print(queue[-2:])

            if current.solved():
                return current

            # Try the next edge until we find one that works
            child = copy.deepcopy(current)
            for src, dst in current_edge_iter:

                # If we successfully add a edge, look for a solution on that branch
                # Store the current state and edge iterator back on the queue
                if child.add_edge(src, dst):

                    queue.append((current, current_edge_iter))
                    if not child in seen:
                        queue.append((child, edge_iter(child)))

                    break

                # Otherwise, ignore this child and try the next node pair

        return False

# --- iterators ---

def shuffled(h):
    '''Return the src and dst in a random order, ignoring if we can even use those nodes'''

    src_nodes = list(h.nodes.keys())
    dst_nodes = copy.copy(src_nodes)

    random.shuffle(src_nodes)
    random.shuffle(dst_nodes)

    for src in src_nodes:
        for dst in dst_nodes:
            if src < dst:
                yield (src, dst)

def least_needed(h):
    '''Return the src and dst that together need the least edges.'''

    src_nodes = list(h.nodes.keys())
    dst_nodes = copy.copy(src_nodes)

    scored_pairs = [
        (src, dst, h.capacity[src] + h.capacity[dst])
        for src in src_nodes
        for dst in dst_nodes
    ]

    for src, dst, score in sorted(scored_pairs, key = lambda el : -el[2]):
        yield (src, dst)

# --- testing ---

with open('easy-1.hashi', 'r') as fin:
    h = Hashi(fin)

print(h)

Hashi.solve(h, shuffled)

print(h)
