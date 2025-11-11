export interface CreditCard {
  cardNumber: string;
  maskedCardNumber: string;
  formattedCardNumber: string;
  accountId: string;
  cardStatus: string;
  cardStatusDisplayName: string;
  isActive: boolean;
  isBlocked: boolean;
  isClosed: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface CreditCardFilterRequest {
  cardNumber?: string;
  accountId?: string;
  cardStatus?: string;
  page?: number;
  size?: number;
}

export interface CreditCardListResponse {
  content: CreditCard[];
  totalElements: number;
  totalPages: number;
  size: number;
  number: number;
  first: boolean;
  last: boolean;
}

export interface Account {
  accountId: string;
  creditCardCount: number;
  userAccessCount: number;
  createdAt: string;
  updatedAt: string;
}

export interface User {
  userId: string;
  userType: 'ADMIN' | 'REGULAR';
  userTypeDisplayName: string;
  isAdmin: boolean;
  canViewAllCards: boolean;
  requiresAccountContext: boolean;
  accessibleAccountIds: string[];
  accessibleAccountCount: number;
  createdAt: string;
  updatedAt: string;
}

export interface ValidationError {
  field: string;
  message: string;
  code: string;
}

export interface CreditCardFilterValidation {
  isValid: boolean;
  errors: ValidationError[];
}

export type CardStatusCode = 'A' | 'I' | 'B' | 'P' | 'C' | 'S' | 'E' | 'L' | 'T' | 'D';

export const CARD_STATUS_OPTIONS: { value: CardStatusCode; label: string }[] = [
  { value: 'A', label: 'Active' },
  { value: 'I', label: 'Inactive' },
  { value: 'B', label: 'Blocked' },
  { value: 'P', label: 'Pending' },
  { value: 'C', label: 'Closed' },
  { value: 'S', label: 'Suspended' },
  { value: 'E', label: 'Expired' },
  { value: 'L', label: 'Lost' },
  { value: 'T', label: 'Stolen' },
  { value: 'D', label: 'Damaged' },
];

export const ERROR_MESSAGES = {
  CARD_FILTER_INVALID: 'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER',
  CARD_FILTER_BLANK: 'CARD FILTER CANNOT BE BLANK, SPACES OR ZEROS',
  ACCOUNT_FILTER_INVALID: 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER',
  ACCOUNT_FILTER_BLANK: 'ACCOUNT FILTER CANNOT BE BLANK, SPACES OR ZEROS',
  MULTIPLE_SELECTION: 'MORE THAN ONE ACTION SELECTED',
  INVALID_ACTION_CODE: 'INVALID ACTION CODE',
  NO_PREVIOUS_PAGES: 'NO PREVIOUS PAGES TO DISPLAY',
  NO_MORE_PAGES: 'NO MORE PAGES TO DISPLAY',
  NO_MORE_RECORDS: 'NO MORE RECORDS TO SHOW',
  NO_RECORDS_FOUND: 'NO RECORDS FOUND FOR THIS SEARCH CONDITION',
  FILE_OPERATION_ERROR: 'File operation error with details',
};
