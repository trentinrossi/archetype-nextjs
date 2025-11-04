// User types for User and Security Administration

export interface User {
  userId: string;
  firstName: string;
  lastName: string;
  password?: string;
  userType: 'A' | 'R';
  createdAt: string;
  updatedAt: string;
}

export interface CreateUserRequest {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: 'A' | 'R';
}

export interface UpdateUserRequest {
  firstName: string;
  lastName: string;
  password: string;
  userType: 'A' | 'R';
}

export interface UserListResponse {
  content: User[];
  pageNumber: number;
  pageSize: number;
  totalElements: number;
  totalPages: number;
  first: boolean;
  last: boolean;
  hasNext: boolean;
  hasPrevious: boolean;
}

export interface UserResponse {
  user: User;
  message: string;
}

export interface UserDeleteResponse {
  message: string;
}
