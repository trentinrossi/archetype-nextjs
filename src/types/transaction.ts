export interface Transaction {
  transactionId: string;
  tranId: number;
  tranTypeCd: number;
  tranCatCd: number;
  tranSource: string;
  tranDesc: string;
  tranAmt: number;
  tranCardNum: string;
  tranMerchantId: string;
  tranMerchantName: string;
  tranMerchantCity: string;
  tranMerchantZip: string;
  tranOrigTs: string;
  tranProcTs: string;
  formattedTransactionDate: string;
  formattedTransactionAmount: string;
  createdAt: string;
  updatedAt: string;
}

export interface TransactionListPage {
  content: Transaction[];
  pageNumber: number;
  totalPages: number;
  totalElements: number;
  hasNextPage: boolean;
  hasPreviousPage: boolean;
}

export interface CreateTransactionRequest {
  transactionId: string;
  tranTypeCd: number;
  tranCatCd: number;
  tranSource: string;
  tranDesc: string;
  tranAmt: number;
  tranCardNum: string;
  tranMerchantId: string;
  tranMerchantName: string;
  tranMerchantCity: string;
  tranMerchantZip: string;
  tranOrigTs: string;
  tranProcTs: string;
}

export interface UpdateTransactionRequest {
  tranTypeCd?: number;
  tranCatCd?: number;
  tranSource?: string;
  tranDesc?: string;
  tranAmt?: number;
  tranCardNum?: string;
  tranMerchantId?: string;
  tranMerchantName?: string;
  tranMerchantCity?: string;
  tranMerchantZip?: string;
  tranOrigTs?: string;
  tranProcTs?: string;
}

export interface TransactionSearchParams {
  startTransactionId?: string;
  pageNumber?: number;
}

export interface TransactionReportRequest {
  reportType: 'MONTHLY' | 'YEARLY' | 'CUSTOM';
  startDate?: string;
  endDate?: string;
}
