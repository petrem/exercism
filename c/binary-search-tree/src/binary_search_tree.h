#ifndef _BINARY_SEARCH_TREE_H
#define _BINARY_SEARCH_TREE_H

typedef struct node {
  int data;
  struct node *left;
  struct node *right;
} node_t;

node_t *build_tree(int data[], int n_data);
void free_tree(node_t *);
int *sorted_data(node_t *);

struct array_info {
  int *array;
  int len;
};

typedef enum {
  PREORDER,
  INORDER,
  POSTORDER
} order_t;

#endif
