export interface Account {
  id: number;
  account_id: number;
  active_status: string;
  current_balance: number;
  credit_limit: number;
  cash_credit_limit: number;
  open_date: string;
  expiration_date: string;
  reissue_date: string;
  current_cycle_credit: number;
  current_cycle_debit: number;
  group_id?: string;
}

export interface AccountSearchRequest {
  account_id: number;
}

export interface UpdateAccountRequest {
  account_id?: number;
  active_status?: string;
  current_balance?: number;
  credit_limit?: number;
  cash_credit_limit?: number;
  open_date?: string;
  expiration_date?: string;
  reissue_date?: string;
  current_cycle_credit?: number;
  current_cycle_debit?: number;
  group_id?: string;
}

export interface AccountWithCustomer {
  account: Account;
  customer: Customer;
}

export interface Customer {
  id: number;
  customer_id: number;
  cust_id: number;
  ssn: string;
  first_name: string;
  middle_name?: string;
  last_name: string;
  address_line_1: string;
  address_line_2?: string;
  address_line_3: string;
  city: string;
  state_code: string;
  country_code: string;
  zip_code: string;
  phone_number_1: string;
  phone_number_2?: string;
  date_of_birth: string;
  government_issued_id?: string;
  government_id?: string;
  eft_account_id?: string;
  primary_holder_indicator: string;
  primary_card_holder_indicator: string;
  fico_score: number;
  customer_number: number;
}

export interface UpdateCustomerRequest {
  customer_id?: number;
  ssn?: string;
  first_name?: string;
  middle_name?: string;
  last_name?: string;
  address_line_1?: string;
  address_line_2?: string;
  address_line_3?: string;
  city?: string;
  state_code?: string;
  country_code?: string;
  zip_code?: string;
  phone_number_1?: string;
  phone_number_2?: string;
  date_of_birth?: string;
  government_issued_id?: string;
  government_id?: string;
  eft_account_id?: string;
  primary_holder_indicator?: string;
  primary_card_holder_indicator?: string;
  fico_score?: number;
}
