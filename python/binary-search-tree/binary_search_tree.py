"""Exercism: binary-search-tree.

Note this is a "creative" implementation, it is not meant to be useful in any way.
It just explores a bit, without being good.

Instead of a "classic" implementation with objects, I use an array
(a list, really) to keep the data, along with left and right indexes.

Another thing to explore would be to add back-references instead of modifying
the already existing nodes, thus making array strictly append-only.

Note his implementation also avoids recursion, given that python
is not well-suited for deep recursive calls.
"""
import operator
from collections import namedtuple
from functools import partial, reduce
from itertools import repeat
from typing import Optional


class TreeNode:

    """Tree Node as needed for exercise."""

    def __init__(self, data, left=None, right=None):
        self.data = data
        self.left = left
        self.right = right

    def __repr__(self):
        return f"TreeNode(data={self.data}, left={self.left}, right={self.right})"


class BinarySearchTree:

    """An adapter for the Exercism exercise tests."""

    def __init__(self, tree_data: list):
        self._bst = BST(tree_data)

    # previous iteration typing annotation failed as foo|bar isn't yet valid
    # in Exercism's world. Upgrade, we're almost in 2022! ;-)
    def data(self) -> Optional[TreeNode]:
        """Build a TreeNode out of the underlying BST.

        This requirement makes the current approach rather ridiculous, but
        on the other hand this would have been boring to implement as intended
        by the exercise.

        The subtree-making was fun indeed!
        """
        nodes = [None] * len(self._bst)
        for node, index in self._bst.post_order_with_index():
            left = nodes[node.left] if node.left is not None else None
            right = nodes[node.right] if node.right is not None else None
            subtree = reduce(
                partial(TreeNode, node.value),
                [*repeat(None, node.count - 1), right],
                left,
            )
            nodes[index] = subtree
        return nodes[0]

    def sorted_data(self) -> list:
        """Get sorted data as a list."""
        return [n.value for n in self._bst.in_order() for _ in range(n.count)]


class BST:

    """Binary Search Tree implemented using an underlying append-only list."""

    # Since we modify the left/right indexes, the node should not be a tuple
    _Node = namedtuple("_Node", "value, left, right, count", defaults=[None, None, 1])

    def __init__(self, iterable):
        self._array = []
        for value in iterable:
            self.insert(value)

    def __iter__(self):
        """Return an iterator over the values in insert order."""
        return map(operator.attrgetter("value"), self._array)

    def __repr__(self):
        return f"BST([{','.join(str(v) for v in self)}])"

    def __len__(self):
        return len(self._array)

    def insert(self, value):
        """Insert value in BST (if not already present)."""
        index, parent = self._find(value)
        if index is None:
            # wasn't found
            append_index = len(self._array)
            self._array.append(self._Node(value))
            if parent is not None:
                # there exists a previous node for the new value
                parent_node = self._array[parent]
                if parent_node.value < value:
                    self._array[parent] = parent_node._replace(right=append_index)
                else:
                    self._array[parent] = parent_node._replace(left=append_index)
        else:
            # was found
            node = self._array[index]
            self._array[index] = node._replace(count=node.count + 1)

    def _find(self, value):
        if self._array:
            index = 0
            previous = None
            while index is not None:
                node = self._array[index]
                if node.value == value:
                    break
                previous = index
                index = node.left if value <= node.value else node.right
            return index, previous
        else:
            return None, None

    def in_order(self):
        """Return an iterator over the nodes ordered by values."""
        # I'm pretty sure this could be done better...
        if not self._array:
            return
        stack = []
        visited = set()
        node = self._array[0]
        while True:
            if node.left is not None:
                left = self._array[node.left]
                if left not in visited:
                    stack.append(node)
                    node = left
                    continue
            if node not in visited:
                yield node
                visited.add(node)
                if node.right is not None:
                    node = self._array[node.right]
                    if node not in visited:
                        continue
            if stack:
                node = stack.pop()
            else:
                break

    def post_order_with_index(self):
        """Return an iterator over the nodes and indexes in post-order."""
        if not self._array:
            return
        stack = []
        visited = set()
        node, index = self._array[0], 0
        while True:
            if node.left is not None:
                left = self._array[node.left]
                if left not in visited:
                    stack.append((node, index))
                    node, index = left, node.left
                    continue
            if node.right is not None:
                right = self._array[node.right]
                if right not in visited:
                    stack.append((node, index))
                    node, index = right, node.right
                    continue
            if node not in visited:
                yield node, index
                visited.add(node)
            if stack:
                node, index = stack.pop()
            else:
                break
