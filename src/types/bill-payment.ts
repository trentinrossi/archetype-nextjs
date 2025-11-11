export interface Account {
  accountId: string;
  currentBalance: number;
  hasPositiveBalance: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface AccountBalance {
  accountId: string;
  currentBalance: number;
  hasPositiveBalance: boolean;
}

export interface ProcessPaymentRequest {
  accountId: string;
  cardNumber: string;
  confirmPayment?: string;
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
  currentBalance?: number;
  hasPositiveBalance?: boolean;
}

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

export interface CardCrossReference {
  accountId: string;
  cardNumber: string;
  createdAt: string;
  updatedAt: string;
}
