// src/types/account.ts

export interface Account {
  accountId: string;
  activeStatus: string;
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  addressZipCode: string;
  groupId: string;
}

export interface Card {
  cardNumber: string;
  accountId: string;
  cvvCode: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: string;
}

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
}

export interface InterestCalculation {
  accountId: string;
  balance: number;
  interestRate: number;
  monthlyInterest: number;
  transactionTypeCode: string;
  transactionCategoryCode: string;
}

export interface AccountStatement {
  customer: Customer;
  account: Account;
  transactions: Transaction[];
  totalAmount: number;
}

export interface CreateAccountData {
  accountId: string;
  activeStatus: string;
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  addressZipCode: string;
  groupId: string;
}

export interface UpdateAccountData {
  activeStatus?: string;
  currentBalance?: number;
  creditLimit?: number;
  cashCreditLimit?: number;
  expirationDate?: string;
  reissueDate?: string;
  currentCycleCredit?: number;
  currentCycleDebit?: number;
  addressZipCode?: string;
  groupId?: string;
}

export interface CreateCardData {
  cardNumber: string;
  accountId: string;
  cvvCode: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: string;
}

export interface UpdateCardData {
  cvvCode?: string;
  embossedName?: string;
  expirationDate?: string;
  activeStatus?: string;
}

export interface CreateCustomerData {
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

export interface UpdateCustomerData {
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
  governmentIssuedId?: string;
  eftAccountId?: string;
  primaryCardholderIndicator?: string;
  ficoCreditScore?: number;
}

export interface CreateTransactionData {
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
}

export interface ApiResponse<T> {
  data: T;
  status: number;
  message?: string;
}

export interface ApiError {
  error: string;
  status: number;
  details?: string;
}
