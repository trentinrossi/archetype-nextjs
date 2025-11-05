export interface Transaction {
  id: number;
  transactionId: string;
  cardNumber: string;
  typeCode: string;
  categoryCode: string;
  source: string;
  description?: string;
  amount: number;
  merchantId: number;
  merchantName: string;
  merchantCity?: string;
  merchantZip?: string;
  originalTimestamp: string;
  processingTimestamp: string;
  createdAt: string;
  updatedAt: string;
}

export interface CreateTransactionRequest {
  cardNumber: string;
  typeCode: string;
  categoryCode: string;
  source: string;
  description?: string;
  amount: number;
  merchantId: number;
  merchantName: string;
  merchantCity?: string;
  merchantZip?: string;
  originalTimestamp: string;
  processingTimestamp: string;
}

export interface TransactionDateRangeQuery {
  startDate: string;
  endDate: string;
  page?: number;
  size?: number;
}

export interface PaginatedTransactionsResponse {
  content: Transaction[];
  totalElements: number;
  totalPages: number;
  size: number;
  number: number;
}

export interface ApiError {
  error: string;
  status: number;
  details?: string;
}
