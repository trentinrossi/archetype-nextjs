export interface Account {
  accountId: number;
  currentBalance: number;
  creditLimit: number;
  currentCycleCredit: number;
  currentCycleDebit: number;
  expirationDate: string;
  createdAt: string;
  updatedAt: string;
}

export interface CreateAccountRequest {
  accountId: number;
  currentBalance: number;
  creditLimit: number;
  currentCycleCredit: number;
  currentCycleDebit: number;
  expirationDate: string;
}

export interface PaginatedAccountsResponse {
  content: Account[];
  totalElements: number;
  totalPages: number;
  size: number;
  number: number;
}

export interface ApiError {
  error: string;
  status: number;
  details?: string;
}
