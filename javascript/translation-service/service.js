/// <reference path="./global.d.ts" />
// @ts-check

import { NotAvailable } from './errors';

export class TranslationService {
  /**
   * Creates a new service
   * @param {ExternalApi} api the original api
   */
  constructor(api) {
    this.api = api;
  }

  /**
   * Attempts to retrieve the translation for the given text.
   *
   * - Returns whichever translation can be retrieved, regardless the quality
   * - Forwards any error from the translation api
   *
   * @param {string} text
   * @returns {Promise<string>}
   */
  free(text) {
    return this.api.fetch(text)
      .then(response => response.translation);
  }

  /**
   * Batch translates the given texts using the free service.
   *
   * - Resolves all the translations (in the same order), if they all succeed
   * - Rejects with the first error that is encountered
   * - Rejects with a BatchIsEmpty error if no texts are given
   *
   * @param {string[]} texts
   * @returns {Promise<string[]>}
   */
  batch(texts) {
    if (texts.length === 0) {
      return Promise.reject(new BatchIsEmpty());
    }
    return texts.reduce(
      (accPromise, text) => this.api.fetch(text)
        .then(
          api_response => accPromise.then(
            accResult => Promise.resolve([...accResult, api_response.translation])
          )
        )
      , Promise.resolve([]));
  }

  _request(text, retries) {
    var self = this;
    return new Promise((resolve, reject) => {
      this.api.request(text, (error) => {
        if (error === undefined) {
          resolve(undefined);
        } else {
          reject(error);
        }
      });
    })
      .catch(function(error) {
        if (retries > 1) {
          //is there a better way to handle `this` in this case?
          return self._request.bind(self)(text, retries - 1);
        } else {
          throw error;
        }
      });
  }
  /**
   * Requests the service for some text to be translated.
   *
   * Note: the request service is flaky, and it may take up to three times for
   *       it to accept the request.
   *
   * @param {string} text
   * @returns {Promise<void>}
   */
  request(text) {
    return this._request(text, 3);
  }

  fetchWithQuality(text, minimumQuality) {
    return this.api.fetch(text)
      .then(response => {
        if (response.quality >= minimumQuality) {
          return response.translation;
        } else {
          throw new QualityThresholdNotMet(text);
        }
      });
  }
  /**
   * Retrieves the translation for the given text
   *
   * - Rejects with an error if the quality can not be met
   * - Requests a translation if the translation is not available, then retries
   *
   * @param {string} text
   * @param {number} minimumQuality
   * @returns {Promise<string>}
   */
  premium(text, minimumQuality) {
    return this.fetchWithQuality(text, minimumQuality)
      .catch(error => {
        if (error instanceof NotAvailable) {
          return this._request(text, 1).then(
            // eslint-disable-next-line no-unused-vars
            response => this.fetchWithQuality(text, minimumQuality)
          );
        } else {
          throw error;
        }
      });
  }
}

/**
 * This error is used to indicate a translation was found, but its quality does
 * not meet a certain threshold. Do not change the name of this error.
 */
export class QualityThresholdNotMet extends Error {
  /**
   * @param {string} text
   */
  constructor(text) {
    super(
      `
The translation of ${text} does not meet the requested quality threshold.
    `.trim(),
    );

    this.text = text;
  }
}

/**
 * This error is used to indicate the batch service was called without any
 * texts to translate (it was empty). Do not change the name of this error.
 */
export class BatchIsEmpty extends Error {
  constructor() {
    super(
      `
Requested a batch translation, but there are no texts in the batch.
    `.trim(),
    );
  }
}
