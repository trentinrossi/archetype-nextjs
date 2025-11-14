export interface AccountValidationRequest {
  acctId: string;
}

export interface AccountValidationResponse {
  acctId: string;
  valid: boolean;
  message: string;
}

export interface BalanceCheckRequest {
  acctId: string;
}

export interface BalanceCheckResponse {
  acctId: string;
  hasBalance: boolean;
  currentBalance: number;
  message: string;
}

export interface ProcessPaymentRequest {
  acctId: string;
}

export interface ProcessPaymentResponse {
  acctId: string;
  paymentAmount: number;
  newBalance: number;
  status: string;
  message: string;
}

export interface BillPaymentFormData {
  accountId: string;
  confirmation: string;
}

export interface BillPaymentState {
  accountId: string;
  currentBalance: number | null;
  confirmation: string;
  isAccountValid: boolean;
  hasBalance: boolean;
  isProcessing: boolean;
  errorMessage: string | null;
  successMessage: string | null;
  transactionId: string | null;
}
