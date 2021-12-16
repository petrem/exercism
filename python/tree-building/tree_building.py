class Record():
    def __init__(self, record_id, parent_id):
        self.record_id = record_id
        self.parent_id = parent_id

    def __str__(self):
        return f"({self.record_id}, {self.parent_id})"

    def __repr__(self):
        return f"Record{self}"

    def __lt__(self, other):
        return self.record_id < other.record_id

    def is_root(self):
        return self.parent_id == self.record_id == 0

    def is_valid(self):
        return self.is_root() or (
            self.record_id > self.parent_id and self.parent_id >= 0
        )


class Node():
    def __init__(self, node_id):
        self.node_id = node_id
        self.children = []

    def __str__(self):
        return f"({self.node_id}, {self.children})"

    def __repr__(self):
        return f"Node{self}"


def BuildTree(records):
    nodes = [None] * len(records)
    for index, record in enumerate(sorted(records)):
        if record.is_valid() and record.record_id == index:
            if nodes[record.parent_id] is None:
                nodes[record.parent_id] = Node(record.parent_id)
            if nodes[record.record_id] is None:
                nodes[record.record_id] = Node(record.record_id)
            if not record.is_root():
                nodes[record.parent_id].children.append(nodes[record.record_id])
        else:
            raise ValueError(
                'Invalid record, first record not root or tree not continuous'
            )
    if nodes:
        return nodes[0]
    return None
