#include "bst.h"
#include <string.h>
#include <stdlib.h>

#ifdef DEBUG
#include <stdio.h>
#endif

struct BST *bst_init(int max_nodes) {
  struct BST *bst = malloc_or_die(sizeof(struct BST));
  bst->keys = calloc_or_die(max_nodes, sizeof(unsigned));
  bst->left = calloc_or_die(max_nodes, sizeof(int));
  bst->right = calloc_or_die(max_nodes, sizeof(int));
  bst->max = max_nodes;
  bst->len = 0;
  return bst;
}

int bst_resize(struct BST *bst, int new_max_nodes) {
  /* unsigned *keys = realloc(bst->keys, new_max_nodes * sizeof(unsigned)); */
  /* if (keys != NULL) */
  /*   bst->keys = keys; */
  /* int *left = realloc(bst->left, new_max_nodes * sizeof(int)); */
  /* if (left != NULL) */
  /*   bst->left = left; */
  /* int *right = realloc(bst->right, new_max_nodes * sizeof(int)); */
  /* if (right != NULL) */
  /*   bst->right = right; */
  int keys = realloc_safe((void **)&bst->keys, bst->max * sizeof(unsigned), new_max_nodes * sizeof(unsigned));
  int left = realloc_safe((void **)&bst->left, bst->max * sizeof(int), new_max_nodes * sizeof(int));
  int right = realloc_safe((void **)&bst->right, bst->max * sizeof(int), new_max_nodes * sizeof(int));
  if (keys == -1 || left == -1 || right == -1)
    return -1;
  bst-> max = new_max_nodes;
  return 0;
}

void bst_destroy(struct BST *bst) {
  if (bst == NULL)
    return;
  if (bst->keys != NULL)
    free(bst->keys);
  if (bst->left != NULL)
    free(bst->left);
  if (bst->right != NULL)
    free(bst->right);
  free(bst);
}

/* bst_find: Look for `key` in binary search tree `bst`.
 * outputs: If not found, will update `*previous` with the last valid index (position to insert).
 * returns: -1 if not found,
 *          index of element if found.
 */
int bst_find(struct BST *bst, unsigned key, int *previous) {
  int i = 0;
  if (bst == NULL)
    return -2;
  if (bst->len == 0) {
    if (previous != NULL)
      *previous = -1;
    return -1;
  }
  int prev = -1;
  do {
    if (bst->keys[i] == key)  /* found */
      return i;
    if (bst->keys[i] < key) {
      prev = i;
      i = bst->right[i];
    } else {
      prev = i;
      i = bst->left[i];
    }
  } while (i > 0);
  if (previous != NULL)
    *previous = prev;
  return -1;
}

/* bst_insert: Add element to binary search tree bst.
 * returns: >= 0  - element was present at the specified index
 *            -1  - element was added
 *            -2 - bst size overflowed
 *            -3 - other errors
 */
int bst_insert(struct BST *bst, unsigned key) {
  if (bst == NULL)
    return -3;
  int previous, found_at;
#ifdef DEBUG
  printf("inserting %u ", key);
#endif
  found_at = bst_find(bst, key, &previous);
  if (found_at >= 0) {
#ifdef DEBUG
    printf("found at %d\n", found_at);
#endif
    return found_at;
  }
  if (found_at != -1) {
    /* a more accurate error reporting would be nice */
#ifdef DEBUG
    printf("bst_find returned error %d\n", found_at);
#endif
    return -3;
  }

  if (bst->len == bst->max) {
#ifdef DEBUG
    printf("too many keys\n");
#endif
    return -2;
  }

  int insert_at = bst->len;
  bst->keys[insert_at] = key;
  bst->len++;

  if (previous == -1) {
    /* first key */
#ifdef DEBUG
    printf("first key\n");
#endif
    return -1;
  }

  if (bst->keys[previous] == key) {  /* should never happen; bst_find lied to us! rudely repport an unknown error */
#ifdef DEBUG
    printf("bst_find did not find the key, but it is equal to the previous?!?\n");
#endif
    return -3;
  }
  if (bst->keys[previous] < key) {  /* insert at previous' right */
    bst->right[previous] = insert_at;
#ifdef DEBUG
    printf("at %d, right of %d\n", insert_at, previous);
#endif
  } else {        /* insert at previous' left */
    bst->left[previous] = insert_at;
#ifdef DEBUG
    printf("at %d, left of %d\n", insert_at, previous);
#endif
  }
  return -1;
}

inline int bst_length(struct BST *bst) {
  return bst->len;
}

inline int bst_get_key_at(struct BST *bst, int pos, unsigned *key) {
  if (bst == NULL || pos < 0 || pos >= bst->len || key == NULL)
    return -1;
  *key = bst->keys[pos];
  return 0;
}

unsigned bst_reduce(struct BST *bst, unsigned (*op)(unsigned, unsigned), unsigned initial) {
  unsigned acc = initial;
  for (int i = 0; i < bst->len; i++)
    acc = op(acc, bst->keys[i]);
  return acc;
}
