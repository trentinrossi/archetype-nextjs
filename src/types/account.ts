export interface Account {
  accountId: number;
  formattedAccountId: string;
  creditCardCount: number;
  createdAt: string;
  updatedAt: string;
}

export interface CreateAccountRequest {
  accountId: string;
}

export interface AccountSearchParams {
  searchTerm?: string;
  page?: number;
  size?: number;
  sort?: string;
}
