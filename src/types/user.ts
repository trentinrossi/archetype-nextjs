export interface User {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: 'A' | 'R';
  createdAt: string;
  updatedAt: string;
}

export interface Transaction {
  transactionId: string;
  transactionDate: string;
  transactionDescription: string;
  transactionAmount: number;
  userId: string;
  userFullName: string;
  createdAt: string;
}

export interface CreateUserRequest {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: 'A' | 'R';
}

export interface UpdateUserRequest {
  firstName?: string;
  lastName?: string;
  password?: string;
  userType?: 'A' | 'R';
}

export interface CreateTransactionRequest {
  transactionId: string;
  transactionDate: string;
  transactionDescription: string;
  transactionAmount: number;
  userId: string;
}

export interface UpdateTransactionRequest {
  transactionDate?: string;
  transactionDescription?: string;
  transactionAmount?: number;
  userId?: string;
}

export interface PaginatedResponse<T> {
  content: T[];
  pageNumber: number;
  pageSize: number;
  totalElements: number;
  totalPages: number;
  first: boolean;
  last: boolean;
  hasNext: boolean;
  hasPrevious: boolean;
}

export interface PaginatedUsersResponse extends PaginatedResponse<User> {}

export interface PaginatedTransactionsResponse extends PaginatedResponse<Transaction> {}

export interface PaginationParams {
  page?: number;
  size?: number;
  sort?: string;
}

export interface UserFilterParams extends PaginationParams {
  userType?: 'A' | 'R';
  firstName?: string;
  lastName?: string;
}

export interface TransactionFilterParams extends PaginationParams {
  userId?: string;
  startDate?: string;
  endDate?: string;
  minAmount?: number;
  maxAmount?: number;
}
