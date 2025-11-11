export interface Transaction {
  transactionId: number;
  transactionTypeCode: string;
  transactionCategoryCode: number;
  transactionSource: string;
  description: string;
  amount: number;
  cardNumber: string;
  merchantId: number;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  originationTimestamp: string;
  processingTimestamp: string;
  accountId: string;
  createdAt: string;
  updatedAt: string;
}

export interface CreateTransactionRequest {
  transactionTypeCode: string;
  transactionCategoryCode: number;
  transactionSource: string;
  description: string;
  amount: number;
  cardNumber: string;
  merchantId: number;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  accountId: string;
}

export interface PaginatedTransactions {
  content: Transaction[];
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
