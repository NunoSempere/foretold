import { rangeRegex, regexBasedFormatter } from '../lib';

export const item = {
  formatterName: 'DISTRIBUTION_PROPORTIONALITY',
  ...regexBasedFormatter(rangeRegex(/of|in/), () => 'BETA'),
};
