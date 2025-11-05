/**
 * Application Configuration
 * 
 * This file contains all configuration settings for the application.
 * Update these values based on your environment.
 */

export const config = {
  /**
   * API Base URL
   * The base URL for all API requests
   * Default: http://localhost:8080/api
   * 
   * For production, update this to your production API URL
   * Example: https://api.yourdomain.com/api
   */
  apiBaseUrl: process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8080/api',

  /**
   * Application Settings
   */
  app: {
    name: 'Card Transaction Lifecycle Management',
    version: '1.0.0',
    description: 'Manage card transactions, accounts, cards, and cross-references',
  },

  /**
   * Pagination Settings
   */
  pagination: {
    defaultPageSize: 20,
    pageSizeOptions: [10, 20, 50, 100],
  },

  /**
   * Date Format Settings
   */
  dateFormat: {
    display: 'MM/DD/YYYY',
    api: 'YYYY-MM-DD',
    timestamp: 'YYYY-MM-DDTHH:mm:ss',
  },

  /**
   * Validation Rules
   */
  validation: {
    cardNumber: {
      length: 16,
      pattern: /^\d{16}$/,
    },
    accountId: {
      maxLength: 11,
      pattern: /^\d{1,11}$/,
    },
    typeCode: {
      length: 2,
      pattern: /^\d{2}$/,
    },
    categoryCode: {
      length: 4,
      pattern: /^\d{4}$/,
    },
    merchantId: {
      maxLength: 9,
      pattern: /^\d{1,9}$/,
    },
    source: {
      maxLength: 10,
    },
    description: {
      maxLength: 100,
    },
    merchantName: {
      maxLength: 50,
    },
  },

  /**
   * Error Codes
   * Transaction validation error codes from COBOL business rules
   */
  errorCodes: {
    INVALID_CARD: 100,
    ACCOUNT_NOT_FOUND: 101,
    OVERLIMIT: 102,
    ACCOUNT_EXPIRED: 103,
  },

  /**
   * Error Messages
   */
  errorMessages: {
    100: 'Invalid card number. Please verify the card number is correct.',
    101: 'Account not found. The card is not associated with any account.',
    102: 'Transaction exceeds account limit. Please contact support.',
    103: 'Account has expired. Please contact support.',
  },
};

export default config;
