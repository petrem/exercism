#include "alloc.h"
#include "bst.h"
#include "word_count.h"

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


static void strncpy_tolower(char *dest, const char *src, size_t n) {
  while(src!=NULL && *src != '\0' && n--)
    *dest++ = tolower(*src++);
}

static inline int iswordchar(int c) {
  return c == '\'' || isalnum(c);
}

static char *get_word(const char **start) {
  if (start == NULL || *start == NULL || **start == '\0')
    return NULL;
  for(; **start != '\0' && !isalnum(**start); (*start)++);
  const char *end = *start;
  for(; *end != '\0' && iswordchar(*end); end++);
  if (end == *start)
    return NULL;
  /* would be nice to include end-apostrophe for things like "boys' toys" */
  if (*(end-1) == '\'')
    end--;

  long word_len = end - *start;
  char *word = malloc_or_die(word_len + 1);
  strncpy_tolower(word, *start, word_len);
  word[word_len] = '\0';
  *start = end;
  return word;
}

/* copy stuff out to word_cound_word */
static int populate_result(word_count_word_t *words, struct BST *bst) {
  memset(words, 0, sizeof(word_count_word_t)*MAX_WORDS);
  int n_words = bst_length(bst);
  char *key;
  int value;
  for (int i=0; i < n_words; i++) {
    if (bst_get_key_and_value_at(bst, i, &key, &value) == 0) {
#ifdef DEBUG
      printf("%s: %d\n", bst->keys[i], bst->values[i]);
#endif
      strncpy(words[i].text, key, MAX_WORD_LENGTH + 1);
      words[i].count = value;
    }
  }
  bst_destroy(bst);

  return n_words;
}

/* count words and return results */
int word_count(const char *input_text, word_count_word_t *words) {
  char *word;
  const char *start = input_text;
  struct BST * bst = bst_init(MAX_WORDS);
  int value;

  while(NULL != (word = get_word(&start))) {
    if (strlen(word) > MAX_WORD_LENGTH) {
      free(word);
      populate_result(words, bst);
      return EXCESSIVE_LENGTH_WORD;
    }
    int inserted = bst_insert(bst, word, 1);
    //volatile int inserted = -2;
    switch (inserted) {
    case -2:
      free(word);
      populate_result(words, bst);
      return EXCESSIVE_NUMBER_OF_WORDS;
    case -1:
      /* initialized with count 1 */
      break;
    case -3:
      /* we're gauche again... */
      exit(-1);
    default:
      /* increment count */
      if (bst_get_value(bst, word, &value) == 0)
        bst_update_value(bst, word, value + 1);
      else
        /* rude, rude */
        exit(-1);
      break;
    }
    free(word);
  }

  return populate_result(words, bst);
}
