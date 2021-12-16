#ifndef WORD_COUNT_BST_H
#define WORD_COUNT_BST_H

#include "alloc.h"

/* A binary search tree, stored in fixed-length arrays,
 * conveniently having string keys and int values.
 */

struct BST {
  int max;   /* available size */
  int len;   /* current number of nodes */
  char **keys;
  int *values;
  int *left;
  int *right;
};

struct BST_kv {
  char *key;
  int value;
};

struct BST *bst_init(int max_nodes);

void bst_destroy(struct BST *bst);

/* bst_find: Look for `key` in binary search tree `bst`.
 * outputs: If not found, will update `*previous` with the last valid index (position to insert).
 * returns: -1 if not found,
 *          index of element if found.
 */
int bst_find(struct BST *bst, const char *key, int *previous);

/* bst_insert: Add element to binary search tree bst.
 * returns: >= 0  - element was present at the specified index
 *            -1  - element was added
 *            -2 - bst size overflowed
 *            -3 - other errors
 */
int bst_insert(struct BST *bst, const char *key, int value);

int bst_get_value(struct BST *bst, const char *key, int *value);

int bst_update_value(struct BST *bst, const char *key, int value);

int bst_length(struct BST *bst);

int bst_get_key_and_value_at(struct BST *bst, int pos, char **key, int *value);

#endif
