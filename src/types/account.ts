export interface Account {
  accountId: string;
  currentBalance: number;
  hasPositiveBalance: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface CreateAccountRequest {
  accountId: string;
  currentBalance: number;
}

export interface UpdateAccountRequest {
  currentBalance: number;
}

export interface AccountBalance {
  accountId: string;
  currentBalance: number;
  hasPositiveBalance: boolean;
}

export interface ProcessPaymentRequest {
  accountId: string;
  cardNumber: string;
  confirmPayment?: 'Y' | 'N' | '';
}

export interface ProcessPaymentResponse {
  transactionId?: number;
  accountId: string;
  previousBalance?: number;
  newBalance?: number;
  paymentAmount?: number;
  timestamp?: string;
  message: string;
  transactionTypeCode?: string;
  transactionCategoryCode?: number;
  transactionSource?: string;
  transactionDescription?: string;
  merchantId?: number;
  merchantName?: string;
  merchantCity?: string;
  merchantZip?: string;
}

export interface PaginatedAccounts {
  content: Account[];
  pageable: {
    pageNumber: number;
    pageSize: number;
    sort: {
      sorted: boolean;
      unsorted: boolean;
      empty: boolean;
    };
    offset: number;
    paged: boolean;
    unpaged: boolean;
  };
  totalElements: number;
  totalPages: number;
  last: boolean;
  size: number;
  number: number;
  sort: {
    sorted: boolean;
    unsorted: boolean;
    empty: boolean;
  };
  numberOfElements: number;
  first: boolean;
  empty: boolean;
}
