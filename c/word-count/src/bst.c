#include "bst.h"
#include <string.h>

#ifdef DEBUG
#include <stdio.h>
#endif

struct BST *bst_init(int max_nodes) {
  struct BST *bst = malloc_or_die(sizeof(struct BST));
  bst->keys = calloc_or_die(max_nodes, sizeof(char *));
  bst->values = calloc_or_die(max_nodes, sizeof(int));
  bst->left = calloc_or_die(max_nodes, sizeof(int));
  bst->right = calloc_or_die(max_nodes, sizeof(int));
  bst->max = max_nodes;
  bst->len = 0;
  return bst;
}

void bst_destroy(struct BST *bst) {
  if (bst == NULL)
    return;
  if (bst->keys != NULL) {
    if (bst->len > 0)
      for (int i = 0; i < bst->len; i++)
        if (bst->keys[i] != NULL)
          free(bst->keys[i]);
  }
    free(bst->keys);
  if (bst->values != NULL)
    free(bst->values);
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
int bst_find(struct BST *bst, const char *key, int *previous) {
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
    int cmp = strcmp(bst->keys[i], key);
    if (cmp == 0)  /* found */
      return i;
    if (cmp < 0) {
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
int bst_insert(struct BST *bst, const char *key, int value) {
  if (bst == NULL)
    return -3;
  int previous, found_at;
#ifdef DEBUG
  printf("inserting %s: %d -> ", key, value);
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
    printf("too many words\n");
#endif
    return -2;
  }

  int insert_at = bst->len;
  bst->keys[insert_at] = malloc_or_die(strlen(key) + 1);
  strcpy(bst->keys[insert_at], key);
  bst->values[insert_at] = value;
  bst->len++;

  if (previous == -1) {
    /* first key */
#ifdef DEBUG
    printf("first key\n");
#endif
    return -1;
  }

  int cmp = strcmp(bst->keys[previous], key);
  if (cmp == 0) {  /* should never happen; bst_find lied to us! rudely repport an unknown error */
#ifdef DEBUG
    printf("bst_find did not find the key, but it is equal to the previous?!?\n");
#endif
    return -3;
  }
  if (cmp < 0) {  /* insert at previous' right */
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

int bst_get_value(struct BST *bst, const char *key, int *value) {
    int found_at = bst_find(bst, key, NULL);
    if (found_at >= 0) {
      if (value != NULL)
        *value = bst->values[found_at];
      return 0;
    }
    return -1;
}

int bst_update_value(struct BST *bst, const char *key, int value) {
    int found_at = bst_find(bst, key, NULL);
    if (found_at >= 0) {
      bst->values[found_at] = value;
      return 0;
    }
    return -1;
}

inline int bst_length(struct BST *bst) {
  return bst->len;
}

/* bst_iterate: Iterates, in order of addition, over */

inline int bst_get_key_and_value_at(struct BST *bst, int pos, char **key, int *value) {
  if (bst == NULL || pos < 0 || pos >= bst->len || key == NULL || value == NULL)
    return -1;
  *key = bst->keys[pos];
  *value = bst->values[pos];
  return 0;
}
