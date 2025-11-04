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
  createdAt?: string;
  updatedAt?: string;
}

export interface CreateCustomerRequest {
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
