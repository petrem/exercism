// eslint-disable-next-line no-extra-boolean-cast
export const isIsogram = (phrase) => !Boolean(phrase.match(/([a-z]).*\1/i));
