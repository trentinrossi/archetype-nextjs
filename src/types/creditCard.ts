export interface CreditCard {
  cardNumber: string;
  formattedCardNumber: string;
  maskedCardNumber: string;
  accountId: number;
  embossedName: string;
  expirationDate: string;
  expirationDateFormatted: string;
  expiryMonth: string;
  expiryYear: string;
  expirationMonth: number;
  expirationYear: number;
  expirationDay: number;
  activeStatus: string;
  cardStatus: string;
  isActive: boolean;
  isExpired: boolean;
  cvvCode: string;
  version: number;
  lastModifiedBy?: string;
  createdAt: string;
  updatedAt: string;
}

export interface CreateCreditCardRequest {
  cardNumber: string;
  accountId: number;
  embossedName: string;
  expirationDate: string;
  cardStatus: string;
  cvvCode: string;
}

export interface UpdateCreditCardRequest {
  embossedName?: string;
  cardStatus?: string;
  expirationDate?: string;
  expirationMonth?: number;
  expirationYear?: number;
  expirationDay?: number;
  version: number;
  updatedBy?: string;
}

export interface CreditCardSearchParams {
  accountId?: string;
  cardNumber?: string;
  page?: number;
  size?: number;
  sort?: string;
}

export interface CreditCardDetailsParams {
  accountId: string;
  cardNumber: string;
}

export interface PageContext {
  screenNumber: number;
  firstCardNumber: string;
  lastCardNumber: string;
  nextPageIndicator: string;
  lastPageDisplayed: number;
}
