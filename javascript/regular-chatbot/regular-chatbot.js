// @ts-check

/**
 * Given a certain command, help the chatbot recognize whether the command is valid or not.
 *
 * @param {string} command
 * @returns {boolean} whether or not is the command valid
 */

export function isValidCommand(command) {
  return (/^chatbot/i).test(command);
}


const emojiRe = new RegExp(/emoji\d+/, 'g');

/**
 * Given a certain message, help the chatbot get rid of all the emoji's encryption through the message.
 *
 * @param {string} message
 * @returns {string} The message without the emojis encryption
 */
export function removeEmoji(message) {
  return message.replace(emojiRe, '');
}

const phoneRe = /^\(\+\d\d\) \d{3}-\d{3}-\d{3}$/;

/**
 * Given a certain phone number, help the chatbot recognize whether it is in the correct format.
 *
 * @param {string} number
 * @returns {string} the Chatbot response to the phone Validation
 */
export function checkPhoneNumber(number) {
  if (phoneRe.test(number)) {
    return "Thanks! You can now download me to your phone.";
  } else {
    return `Oops, it seems like I can't reach out to ${number}`;
  }
}

const urlRe = /(?:https?:\/\/)?(?:\w+\.)+\w{2,}/g;
/**
 * Given a certain response from the user, help the chatbot get only the URL.
 *
 * @param {string} userInput
 * @returns {string[] | null} all the possible URL's that the user may have answered
 */
export function getURL(userInput) {
  return userInput.match(urlRe);
}

const nameRe = /(\w+)\s*,\s*(\w+)/;
/**
 * Greet the user using the full name data from the profile.
 *
 * @param {string} fullName
 * @returns {string} Greeting from the chatbot
 */
export function niceToMeetYou(fullName) {
  // eslint-disable-next-line no-unused-vars
  const [_full, lastName, firstName] = fullName.match(nameRe);
  return `Nice to meet you, ${firstName} ${lastName}`;
}
