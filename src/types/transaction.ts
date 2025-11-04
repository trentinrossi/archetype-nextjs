// Transaction types for User and Security Administration

export interface Transaction {
  transactionId: string;
  transactionDate: string;
  transactionDescription: string;
  transactionAmount: number;
  userId: string;
  userFullName: string;
  createdAt: string;
}

export interface TransactionListResponse {
  content: Transaction[];
  pageNumber: number;
  pageSize: number;
  totalElements: number;
  totalPages: number;
  first: boolean;
  last: boolean;
  hasNext: boolean;
  hasPrevious: boolean;
}

export interface DateRangeParams {
  startDate: string;
  endDate: string;
}

export interface TransactionReportResponse {
  transactions: Transaction[];
  message: string;
}
