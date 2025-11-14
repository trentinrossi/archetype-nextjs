export interface Account {
  accountId: number;
  activeStatus: string;
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  groupId?: string;
  customerId: number;
  accountStatus?: string;
  availableCredit?: number;
  creditUtilization?: number;
  isOverLimit?: boolean;
  daysUntilExpiration?: number;
  accountAge?: number;
}

export interface CreateAccountRequest {
  accountId: number;
  activeStatus: string;
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  groupId?: string;
  customerId: number;
}

export interface UpdateAccountRequest {
  activeStatus?: string;
  currentBalance?: number;
  creditLimit?: number;
  cashCreditLimit?: number;
  openDate?: string;
  expirationDate?: string;
  reissueDate?: string;
  currentCycleCredit?: number;
  currentCycleDebit?: number;
  groupId?: string;
}

export interface AccountWithCustomer {
  account: Account;
  customer: Customer;
}

export interface Customer {
  customerId: number;
  firstName: string;
  middleName?: string;
  lastName: string;
  addressLine1: string;
  addressLine2?: string;
  addressLine3: string;
  stateCode: string;
  countryCode: string;
  zipCode: string;
  phoneNumber1: string;
  phoneNumber2?: string;
  ssn: number;
  governmentIssuedId?: string;
  dateOfBirth: string;
  eftAccountId?: string;
  primaryCardholderIndicator: string;
  ficoScore: number;
  fullName?: string;
  maskedSsn?: string;
  age?: number;
  fullAddress?: string;
  ficoScoreCategory?: string;
}

export interface CreateCustomerRequest {
  customerId: number;
  firstName: string;
  middleName?: string;
  lastName: string;
  addressLine1: string;
  addressLine2?: string;
  addressLine3: string;
  stateCode: string;
  countryCode: string;
  zipCode: string;
  phoneNumber1: string;
  phoneNumber2?: string;
  ssn: number;
  governmentIssuedId?: string;
  dateOfBirth: string;
  eftAccountId?: string;
  primaryCardholderIndicator: string;
  ficoScore: number;
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
  ssn?: number;
  governmentIssuedId?: string;
  dateOfBirth?: string;
  eftAccountId?: string;
  primaryCardholderIndicator?: string;
  ficoScore?: number;
}
