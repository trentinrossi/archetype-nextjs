// src/types/cardDemo.ts

// Account Types
export interface Account {
  accountId: string;
  activeStatus: string;
  currentBalance: string;
  creditLimit: string;
  cashCreditLimit: string;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: string;
  currentCycleDebit: string;
  addressZipCode: string;
  groupId: string;
}

export interface CreateAccountRequest {
  activeStatus: string;
  currentBalance: string;
  creditLimit: string;
  cashCreditLimit: string;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: string;
  currentCycleDebit: string;
  addressZipCode: string;
  groupId: string;
}

export interface UpdateAccountRequest {
  activeStatus?: string;
  currentBalance?: string;
  creditLimit?: string;
  cashCreditLimit?: string;
  openDate?: string;
  expirationDate?: string;
  reissueDate?: string;
  currentCycleCredit?: string;
  currentCycleDebit?: string;
  addressZipCode?: string;
  groupId?: string;
}

// Card Types
export interface Card {
  cardNumber: string;
  accountId: string;
  cvvCode: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: string;
}

export interface CreateCardRequest {
  accountId: string;
  cvvCode: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: string;
}

export interface UpdateCardRequest {
  accountId?: string;
  cvvCode?: string;
  embossedName?: string;
  expirationDate?: string;
  activeStatus?: string;
}

// Customer Types
export interface Customer {
  customerId: string;
  firstName: string;
  middleName: string;
  lastName: string;
  addressLine1: string;
  addressLine2: string;
  addressLine3: string;
  stateCode: string;
  countryCode: string;
  zipCode: string;
  phoneNumber1: string;
  phoneNumber2: string;
  ssn: string;
  governmentIssuedId: string;
  dateOfBirth: string;
  eftAccountId: string;
  primaryCardholderIndicator: string;
  ficoCreditScore: number;
}

export interface CreateCustomerRequest {
  firstName: string;
  middleName: string;
  lastName: string;
  addressLine1: string;
  addressLine2: string;
  addressLine3: string;
  stateCode: string;
  countryCode: string;
  zipCode: string;
  phoneNumber1: string;
  phoneNumber2: string;
  ssn: string;
  governmentIssuedId: string;
  dateOfBirth: string;
  eftAccountId: string;
  primaryCardholderIndicator: string;
  ficoCreditScore: number;
}

export interface UpdateCustomerRequest {
  firstName?: string;
  middleName?: string;
  lastName?: string;
  addressLine1?: string;
  addressLine2?: string;
  addressLine3?: string;
  stateCode?: string;
  countryCode?: string;
  zipCode?: string;
  phoneNumber1?: string;
  phoneNumber2?: string;
  ssn?: string;
  governmentIssuedId?: string;
  dateOfBirth?: string;
  eftAccountId?: string;
  primaryCardholderIndicator?: string;
  ficoCreditScore?: number;
}

// Transaction Types
export interface Transaction {
  transactionId: string;
  cardNumber: string;
  accountId: string;
  transactionTypeCode: string;
  transactionCategoryCode: string;
  transactionSource: string;
  transactionDescription: string;
  transactionAmount: string;
  merchantId: string;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  originalTimestamp: string;
  processingTimestamp: string;
}

export interface CreateTransactionRequest {
  cardNumber: string;
  accountId: string;
  transactionTypeCode: string;
  transactionCategoryCode: string;
  transactionSource: string;
  transactionDescription: string;
  transactionAmount: string;
  merchantId: string;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  originalTimestamp: string;
  processingTimestamp: string;
}

// Interest Calculation Types
export interface InterestCalculation {
  accountId: string;
  balance: string;
  interestRate: string;
  monthlyInterest: string;
  transactionTypeCode: string;
  transactionCategoryCode: string;
}

// Account Statement Types
export interface AccountStatement {
  customer: Customer;
  account: Account;
  transactions: Transaction[];
  totalAmount: string;
}
