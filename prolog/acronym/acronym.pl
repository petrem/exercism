abbreviate(Sentence, Acronym):-
    separators(Sep),
    string_upper(Sentence, SentenceUpper)
    , split_string(SentenceUpper, Sep, Sep, Words)
    , words_heads(Words, Heads)
    , string_chars(Acronym, Heads).

separators(" _-").

words_heads([], []).
words_heads([Word|Rest], [Head|Heads]):-
    word_head(Word, Head)
    , words_heads(Rest, Heads).

word_head(Word, Head):-
    string_chars(Word, [Head|_]).
