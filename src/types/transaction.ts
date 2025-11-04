export interface Transaction {
  transactionId: string;
  cardNumber: string;
  accountId: string;
  transactionTypeCode: string;
  transactionCategoryCode: string;
  transactionSource: string;
  transactionDescription: string;
  transactionAmount: number;
  merchantId: number;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  originalTimestamp: string;
  processingTimestamp: string;
  createdAt?: string;
  updatedAt?: string;
}

export interface CreateTransactionRequest {
  transactionId: string;
  cardNumber: string;
  accountId: string;
  transactionTypeCode: string;
  transactionCategoryCode: string;
  transactionSource: string;
  transactionDescription: string;
  transactionAmount: number;
  merchantId: number;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  originalTimestamp: string;
  processingTimestamp: string;
}

export interface UpdateTransactionRequest {
  cardNumber?: string;
  accountId?: string;
  transactionTypeCode?: string;
  transactionCategoryCode?: string;
  transactionSource?: string;
  transactionDescription?: string;
  transactionAmount?: number;
  merchantId?: number;
  merchantName?: string;
  merchantCity?: string;
  merchantZip?: string;
  originalTimestamp?: string;
  processingTimestamp?: string;
}
