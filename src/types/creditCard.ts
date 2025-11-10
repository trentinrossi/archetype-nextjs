export interface CreditCard {
  cardNumber: string;
  maskedCardNumber: string;
  accountId: string;
  cardStatus: string;
  cardStatusEnum: 'ACTIVE' | 'INACTIVE' | 'BLOCKED' | 'CANCELLED' | 'SUSPENDED';
  cardStatusDisplayName: string;
  cardholderName?: string;
  expiryMonth?: string;
  expiryYear?: string;
  cardType?: string;
  creditLimit?: number;
  availableCredit?: number;
  isActive: boolean;
  isExpired: boolean;
  canModify: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface CreateCreditCardRequest {
  cardNumber: string;
  accountId: string;
  cardStatus: string;
  cardholderName?: string;
  expiryMonth?: string;
  expiryYear?: string;
  cardType?: string;
  creditLimit?: number;
  availableCredit?: number;
}

export interface UpdateCreditCardRequest {
  cardStatus?: string;
  cardholderName?: string;
  expiryMonth?: string;
  expiryYear?: string;
  cardType?: string;
  creditLimit?: number;
  availableCredit?: number;
}

export interface CreditCardSearchFilters {
  accountId?: string;
  cardNumberPattern?: string;
  cardStatus?: string;
}

export interface PaginatedCreditCardResponse {
  content: CreditCard[];
  pageable: {
    sort: any;
    offset: number;
    pageNumber: number;
    pageSize: number;
    paged: boolean;
    unpaged: boolean;
  };
  last: boolean;
  totalPages: number;
  totalElements: number;
  size: number;
  number: number;
  sort: any;
  first: boolean;
  numberOfElements: number;
  empty: boolean;
}
