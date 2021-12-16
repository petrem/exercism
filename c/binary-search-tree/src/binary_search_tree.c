#include "binary_search_tree.h"
#include <stdlib.h>
#include <errno.h>

static inline void *create_node(int data);
static void insert_tree(node_t * root, int data);
static void visit_tree(node_t *root, order_t order, void (*visitor)(node_t *, void *), void *arg);
static void free_node(node_t *root, void *arg);
static void add_data(node_t *root, void *arg);


node_t *build_tree(int data[], int n_data) {
  if (data == NULL || n_data == 0)
    return NULL;
  node_t *root = create_node(data[0]);
  for (int i = 1; i < n_data; i++)
    insert_tree(root, data[i]);
  return root;
}


void free_tree(node_t *root) {
  visit_tree(root, POSTORDER, free_node, NULL);
}


int *sorted_data(node_t *root) {
  struct array_info info = {NULL, 0};
  visit_tree(root, INORDER, add_data, &info);
  return info.array;
}


static inline void *create_node(int data) {
  node_t *item;
  item = calloc(1, sizeof(node_t));
  if (item == NULL)
    exit(ENOMEM);
  item->data = data;
  return item;
}


static void insert_tree(node_t *root, int data) {
  if (root->data >= data) {
    if (root->left == NULL)
      root->left = create_node(data);
    else
      insert_tree(root->left, data);
  } else {
    if (root->data < data) {
      if (root->right == NULL)
        root->right = create_node(data);
      else
        insert_tree(root->right, data);
    }
  }
}


static void visit_tree(node_t *root, order_t order, void (*visitor)(node_t *, void *), void * arg) {
  if (root == NULL)
    return;
  if (order == PREORDER)
    visitor(root, arg);
  else
    if (root->left != NULL)
      visit_tree(root->left, order, visitor, arg);
  if (order == INORDER)
    visitor(root, arg);
  if (root->right != NULL)
    visit_tree(root->right, order, visitor, arg);
  if (order == POSTORDER)
    visitor(root, arg);
}


static void free_node(node_t *root, void *arg) {
  (void)(arg); /* suppress unused parameter in callback */
  if (root != NULL)
    free(root);
}


static void add_data(node_t *root, void *arg) {
  struct array_info *info = arg;
  info->array = realloc(info->array, sizeof(int) * (info->len + 1));
  info->array[info->len] = root->data;
  info->len++;
}
