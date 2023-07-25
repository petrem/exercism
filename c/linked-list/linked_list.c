#include <stdlib.h>
#include <stdbool.h>
#include "linked_list.h"

/* Adapted, adopted and well, couldn't possibly improve the implementation in the Linux
   kernel. Used this because it's particularly cool, although in this case not very
   useful, except perhaps because it saves a few `if`s. But hey, a simple linked list is
   boring.
*/

static void *safe_calloc(size_t, size_t);

struct list {
  struct list *prev, *next;
};

struct train_list {
  struct list stations;
  ll_data_t data;
};


/*
 * internal, generic list implementation
 */

static inline bool _list_is_empty(const struct list *head) {
  return head->next == head;
}


static inline bool _list_is_head(const struct list *node, const struct list *head) {
  return node == head;
}


static inline bool _list_is_last(const struct list *node, const struct list *head) {
  return node->next == head;
}


static inline void _list_init(struct list *head) {
  head->next = head;
  head->prev = head;
}


/* This is stolen directly, just adapted sliglty with longer names for clarity and to
   avoid some compile time warnings in the way exercism sets things up.

   The idea is to get a pointer to the containing struct (and cast to that
   struct's type).
 */
#define _list_entry(member_ptr, container_type, member_name)            \
  ((container_type *)((char *)(member_ptr) - offsetof(container_type, member_name)))


static inline void _list_add_between(struct list *new,
                                     struct list *prev,
                                     struct list *next) {
  new->next = next;
  new->prev = prev;
  next->prev = new;
  prev->next = new;
}

static inline void _list_append(struct list *new,
                                struct list * head) {
  _list_add_between(new, head, head->next);
}

static inline void _list_prepend(struct list *new,
                                 struct list * head) {
  _list_add_between(new, head->prev, head);
}

static inline void _list_del_between(struct list *prev,
                                     struct list *next) {
  next->prev = prev;
  prev->next = next;
}

static inline void _list_del(struct list *node) {
  _list_del_between(node->prev, node->next);
  node->prev = NULL;
  node->next = NULL;
}

/*
 * Various ways to iterate over the list. Linux's implementation is more flexible as it
 * uses a macro, but I don't like (C) macros much. Let's try something different.
 */

/* iterate (map?) function over the list */
static inline void _list_iter(const struct list *head,
                              void (callback)(struct list *node, void *args),
                              void *payload) {
  for (struct list *p = head->next; !_list_is_head(p, head); p = p->next)
    callback(p, payload);
}

/* like `_list_iter` but the function take no extra payload */
static inline void _list_iter1(const struct list *head,
                               void (callback)(struct list *node)) {
  for (struct list *p = head->next; !_list_is_head(p, head); p = p->next)
    callback(p);
}

/* like `_list_iter1`, but guard against element removals */
static inline void _list_iter_safe1(const struct list *head,
                                    void (callback)(struct list *node)) {
  for (struct list *p = head->next, *q = p->next;
       !_list_is_head(p, head);
       p = q, q = p->next)
    callback(p);
}

/* like `_list_iter`, except stop when the predicate is true and return a pointer to the
   node */
static inline struct list *
_list_iter_until(const struct list *head,
                 bool (callback)(struct list *node, void *args),
                 void *payload) {
  for (struct list *p = head->next; !_list_is_head(p, head); p = p->next)
    if (callback(p, payload))
      return p;
  return NULL;
}




/* 
 * train stations list interface
 */


struct list *list_create(void) {
  struct list *list_head = safe_calloc(1, sizeof(struct list));
  _list_init(list_head);
  return list_head;
}

static inline void _counter(struct list *node, void *acc) {
  (void)node;
  (*(size_t *)acc)++;
}

// counts the items on a list
size_t list_count(const struct list *list) {
  size_t acc = 0;
  _list_iter(list, _counter, &acc);
  return acc;
}

// inserts item at back of a list
void list_push(struct list *list, ll_data_t item_data) {
  struct train_list *elem = safe_calloc(1, sizeof(struct train_list));
  elem->data = item_data;
  _list_append(&elem->stations, list);
}

// removes item from back of a list
ll_data_t list_pop(struct list *list) {
  if (_list_is_empty(list)) {
    exit(EXIT_FAILURE);
  }
  struct train_list *elem = _list_entry(list->next, struct train_list, stations);
  _list_del(list->next);
  ll_data_t data = elem->data;
  free(elem);
  return data;
}

// inserts item at front of a list
void list_unshift(struct list *list, ll_data_t item_data) {
  struct train_list *elem = safe_calloc(1, sizeof(struct train_list));
  elem->data = item_data;
  _list_prepend(&elem->stations, list);
}

// removes item from front of a list
ll_data_t list_shift(struct list *list) {
  if (_list_is_empty(list)) {
    exit(EXIT_FAILURE);
  }
  struct train_list *elem = _list_entry(list->prev, struct train_list, stations);
  _list_del(list->prev);
  ll_data_t data = elem->data;
  free(elem);
  return data;
}

static bool _compare_data(struct list *node, void *args) {
  struct train_list *entry = _list_entry(node, struct train_list, stations);
  return entry->data == *(ll_data_t *)args;
}

static void _delete_and_free(struct list *node) {
  struct train_list *entry = _list_entry(node, struct train_list, stations);
  _list_del(node);
  free(entry);
}

// deletes a node that holds the matching data
void list_delete(struct list *list, ll_data_t data) {
  struct list *node = _list_iter_until(list, _compare_data, &data);
  if (node)
    _delete_and_free(node);
}

// destroys an entire list
// list will be a dangling pointer after calling this method on it
void list_destroy(struct list *list) {
  _list_iter_safe1(list, _delete_and_free);
  free(list);
}


/*
 * utility functions
 */

static void *safe_calloc(size_t nmemb, size_t size) {
  void *p = calloc(nmemb, size);
  if (p == NULL) {
    exit(EXIT_FAILURE);
  }
  return p;
}
