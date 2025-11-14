export interface Customer {
  customerId: number;
  firstName: string;
  middleName?: string;
  lastName: string;
  fullName?: string;
  addressLine1: string;
  addressLine2?: string;
  addressLine3: string;
  stateCode: string;
  countryCode: string;
  zipCode: string;
  phoneNumber1: string;
  phoneNumber2?: string;
  ssn: string;
  ssnRaw?: number;
  governmentIssuedId?: string;
  dateOfBirth: string;
  age?: number;
  eftAccountId?: string;
  primaryCardholderIndicator: string;
  primaryCardholderStatus?: string;
  ficoScore: number;
  ficoScoreRating?: string;
  ficoCreditScore: number;
  city: string;
  primaryPhoneNumber: string;
  secondaryPhoneNumber?: string;
  fullAddress?: string;
  meetsAgeRequirement?: boolean;
  createdAt?: string;
  updatedAt?: string;
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
  ficoCreditScore: number;
  city: string;
  primaryPhoneNumber: string;
  secondaryPhoneNumber?: string;
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
  ficoCreditScore?: number;
  city?: string;
  primaryPhoneNumber?: string;
  secondaryPhoneNumber?: string;
}
