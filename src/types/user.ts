export interface User {
  id: number;
  userId: string;
  userType: 'ADMIN' | 'REGULAR';
  userTypeDisplayName: string;
  username: string;
  email: string;
  firstName: string;
  lastName: string;
  fullName: string;
  isAdmin: boolean;
  canViewAllCards: boolean;
  accountCount: number;
  createdAt: string;
  updatedAt: string;
}

export interface CreateUserRequest {
  userId: string;
  userType: 'ADMIN' | 'REGULAR';
  username: string;
  email: string;
  firstName: string;
  lastName: string;
}

export interface UpdateUserRequest {
  userType?: 'ADMIN' | 'REGULAR';
  username?: string;
  email?: string;
  firstName?: string;
  lastName?: string;
}

export interface UserSearchParams {
  searchTerm?: string;
  page?: number;
  size?: number;
  sort?: string;
}
