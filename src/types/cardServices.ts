// src/types/cardServices.ts

// ============================================================================
// Account Types
// ============================================================================

export interface Account {
  accountId: string; // 11 digits
  activeStatus: 'Y' | 'N';
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  groupId: string;
  availableCredit: number;
  active: boolean;
  expired: boolean;
}

export interface CreateAccountRequest {
  accountId: string; // 11 digits
  activeStatus: 'Y' | 'N';
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  groupId: string;
}

export interface UpdateAccountRequest {
  activeStatus?: 'Y' | 'N';
  currentBalance?: number;
  creditLimit?: number;
  cashCreditLimit?: number;
  expirationDate?: string;
  reissueDate?: string;
  currentCycleCredit?: number;
  currentCycleDebit?: number;
  groupId?: string;
}

export interface AccountResponse {
  accountId: string;
  activeStatus: 'Y' | 'N';
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  groupId: string;
  availableCredit: number;
  active: boolean;
  expired: boolean;
}

// ============================================================================
// Customer Types
// ============================================================================

export interface Customer {
  customerId: string; // 9 digits
  firstName: string;
  middleName?: string;
  lastName: string;
  fullName: string;
  addressLine1: string;
  addressLine2?: string;
  city: string;
  stateCode: string; // 2 chars
  zipCode: string; // 5 digits
  countryCode: string; // 3 chars
  phoneNumber1: string;
  phoneNumber2?: string;
  governmentIssuedId: string;
  dateOfBirth: string;
  eftAccountId: string;
  primaryCardHolderIndicator: 'Y' | 'N';
  ficoCreditScore: number; // 300-850
  primaryCardHolder: boolean;
}

export interface CreateCustomerRequest {
  customerId: string; // 9 digits
  firstName: string;
  middleName?: string;
  lastName: string;
  addressLine1: string;
  addressLine2?: string;
  city: string;
  stateCode: string; // 2 chars
  zipCode: string; // 5 digits
  countryCode: string; // 3 chars
  phoneNumber1: string;
  phoneNumber2?: string;
  governmentIssuedId: string;
  dateOfBirth: string;
  eftAccountId: string;
  primaryCardHolderIndicator: 'Y' | 'N';
  ficoCreditScore: number; // 300-850
}

export interface UpdateCustomerRequest {
  firstName?: string;
  middleName?: string;
  lastName?: string;
  addressLine1?: string;
  addressLine2?: string;
  city?: string;
  stateCode?: string; // 2 chars
  zipCode?: string; // 5 digits
  countryCode?: string; // 3 chars
  phoneNumber1?: string;
  phoneNumber2?: string;
  governmentIssuedId?: string;
  dateOfBirth?: string;
  eftAccountId?: string;
  primaryCardHolderIndicator?: 'Y' | 'N';
  ficoCreditScore?: number; // 300-850
}

export interface CustomerResponse {
  customerId: string;
  firstName: string;
  middleName?: string;
  lastName: string;
  fullName: string;
  addressLine1: string;
  addressLine2?: string;
  city: string;
  stateCode: string;
  zipCode: string;
  countryCode: string;
  phoneNumber1: string;
  phoneNumber2?: string;
  governmentIssuedId: string;
  dateOfBirth: string;
  eftAccountId: string;
  primaryCardHolderIndicator: 'Y' | 'N';
  ficoCreditScore: number;
  primaryCardHolder: boolean;
}

// ============================================================================
// Card Types
// ============================================================================

export interface Card {
  cardNumber: string; // 16 digits
  accountId: string; // 11 digits
  embossedName: string;
  expirationDate: string;
  activeStatus: 'Y' | 'N';
  active: boolean;
  expired: boolean;
}

export interface CreateCardRequest {
  cardNumber: string; // 16 digits
  accountId: string; // 11 digits
  embossedName: string;
  expirationDate: string;
  activeStatus: 'Y' | 'N';
}

export interface UpdateCardRequest {
  embossedName?: string;
  expirationDate?: string;
  activeStatus?: 'Y' | 'N';
}

export interface CardResponse {
  cardNumber: string;
  accountId: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: 'Y' | 'N';
  active: boolean;
  expired: boolean;
}

export interface CardListItem {
  cardNumber: string;
  accountId: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: 'Y' | 'N';
  active: boolean;
  expired: boolean;
}

// ============================================================================
// Transaction Types
// ============================================================================

export interface Transaction {
  transactionId: string; // 16 digits
  cardNumber: string; // 16 digits
  typeCode: string; // 2 chars
  categoryCode: string;
  source: string;
  description: string;
  amount: number;
  merchantId: string;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  originationTimestamp: string;
  processingTimestamp: string;
}

export interface CreateTransactionRequest {
  transactionId: string; // 16 digits
  cardNumber: string; // 16 digits
  typeCode: string; // 2 chars
  categoryCode: string;
  source: string;
  description: string;
  amount: number;
  merchantId: string;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  originationTimestamp: string;
  processingTimestamp: string;
}

export interface UpdateTransactionRequest {
  typeCode?: string; // 2 chars
  categoryCode?: string;
  source?: string;
  description?: string;
  amount?: number;
  merchantId?: string;
  merchantName?: string;
  merchantCity?: string;
  merchantZip?: string;
  processingTimestamp?: string;
}

export interface TransactionResponse {
  transactionId: string;
  cardNumber: string;
  typeCode: string;
  categoryCode: string;
  source: string;
  description: string;
  amount: number;
  merchantId: string;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  originationTimestamp: string;
  processingTimestamp: string;
}

// ============================================================================
// Bill Payment Types
// ============================================================================

export interface BillPayment {
  accountId: string;
  currentBalance: number;
  paymentAmount: number;
  transactionId: string;
  confirmPayment: string;
}

export interface CreateBillPaymentRequest {
  accountId: string;
  confirmPayment?: string;
}

export interface BillPaymentResponse {
  accountId: string;
  currentBalance: number;
  paymentAmount: number;
  transactionId: string;
  confirmPayment: string;
}

// ============================================================================
// Validation Types
// ============================================================================

export interface ValidationError {
  field: string;
  message: string;
}

export interface ApiError {
  error: string;
  message?: string;
  status?: number;
  timestamp?: string;
  path?: string;
  validationErrors?: ValidationError[];
}

// ============================================================================
// Pagination Types
// ============================================================================

export interface PaginationParams {
  page?: number;
  size?: number;
  sort?: string;
}

export interface PageResponse<T> {
  content: T[];
  totalElements: number;
  totalPages: number;
  size: number;
  number: number;
  first: boolean;
  last: boolean;
  empty: boolean;
}
