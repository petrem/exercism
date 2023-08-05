const isAnagramOf = (word, candidate) => {
  if ((word === candidate) || word.length !== candidate.length)
    return false;
  const candidateLetters = [...candidate].sort();
  const wordLetters = [...word].sort();
  return wordLetters.every((key, idx) => key === candidateLetters[idx]);
};

export const findAnagrams = (word, candidates) => candidates.filter(
  (candidate) => isAnagramOf(word.toLowerCase(), candidate.toLowerCase()));

