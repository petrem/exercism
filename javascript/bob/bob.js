export const hey = (message) => {
  message = message.trim();
  if (is_silence(message)) {
    return "Fine. Be that way!";
  }
  if (is_shout(message)) {
    if (is_question(message)) {
      return "Calm down, I know what I'm doing!";
    }
    else {
      return "Whoa, chill out!";
    }
  }
  if (is_question(message)) {
    return "Sure.";
  }
  return "Whatever.";
};

const is_question = (phrase) => phrase.endsWith("?");

const is_shout = (phrase) => /[A-Z]/.test(phrase) && !/[a-z]/.test(phrase);

const is_silence = (phrase) => phrase == "";
