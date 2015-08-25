import sys

from collections import defaultdict as ddict

# Points will be stored as (row, col) tuples
# The grid is a mapping of points to the character in that location
# Nodes nodes store their point for fast iteration
# The final solution will be a mapping of nodes to a set of other nodes adjacent to them
grid = ddict(lambda : ' ')
nodes = {}
adjacency = ddict(set)

def is_node(c):
    return c in 'abcdefghijklmnopqrstuvwxyz'

def is_edge(c):
    return c in '|-/\\'

def naturals(i = 0):
    while True:
        yield i
        i += 1

# Skip the number of lines
sys.stdin.readline()

# Read the rest of the grid
for row, line in enumerate(sys.stdin):
    for col, char in enumerate(line):

        pt = (row, col)
        grid[pt] = char

        if is_node(char):
            nodes[char] = (row, col)

# List of possible edges, ordered row, col, edge type
possible_edges = (
    (-1, -1, '\\'), (-1, 0, '|'), (-1, 1, '/'),
    ( 0, -1, '-'),                ( 0, 1, '-'),
    ( 1, -1, '/'),  ( 1, 0, '|'), ( 1, 1, '\\')
)

def neighbors(src_pt, previous_deltas = None):
    '''Given a point, yield any neighboring nodes'''

    src = grid[src_pt]
    row, col = src_pt

    for row_delta, col_delta, edge_type in possible_edges:

        dst_pt = (row + row_delta, col + col_delta)
        dst = grid[dst_pt]

        # Don't go back the way we came
        if (-row_delta, -col_delta) == previous_deltas:
            continue

        # A valid leaving edge, follow it until a node or a #
        elif dst == edge_type:
            for i in naturals(2):
                dst_pt = (row + i * row_delta, col + i * col_delta)
                dst = grid[dst_pt]

                # Found the target node, add
                if is_node(dst):
                    yield dst
                    break

                # Found a connector, continue on the other exit point
                elif dst == '#':
                    yield from neighbors(dst_pt, (row_delta, col_delta))
                    break

# Start at each node and expand all edges
# Note: This will find each edge twice, so it goes
for (src, pt) in nodes.items():
    for dst in neighbors(pt):
        adjacency[src].add(dst)
        adjacency[dst].add(src)

# Print an adjacency matrix in sorted node order
for src in sorted(nodes):
    for dst in sorted(nodes):
        if dst in adjacency[src]:
            sys.stdout.write('1')
        else:
            sys.stdout.write('0')
    sys.stdout.write('\n')
