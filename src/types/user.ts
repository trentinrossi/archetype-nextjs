// src/types/user.ts

export interface User {
  userId: string; // 8 characters
  firstName: string; // max 20 characters
  lastName: string; // max 20 characters
  password: string; // 8 characters
  userType: 'A' | 'U'; // A for Admin, U for User
  createdAt?: string;
  updatedAt?: string;
}

export interface CreateUserRequest {
  firstName: string;
  lastName: string;
  password: string;
  userType: 'A' | 'U';
}

export interface UpdateUserRequest {
  firstName?: string;
  lastName?: string;
  password?: string;
  userType?: 'A' | 'U';
}

export interface SignonRequest {
  userId: string;
  password: string;
}

export interface SignonResponse {
  success: boolean;
  user?: {
    userId: string;
    firstName: string;
    lastName: string;
    userType: 'A' | 'U';
    role: UserRole;
  };
  token?: string;
  message?: string;
}

export interface AuthUser {
  userId: string;
  firstName: string;
  lastName: string;
  userType: 'A' | 'U';
  role: UserRole;
}

export enum UserRole {
  ADMIN = 'ADMIN',
  GENERAL = 'GENERAL'
}

export interface UserValidationError {
  field: string;
  message: string;
}

export interface UserValidationResult {
  isValid: boolean;
  errors: UserValidationError[];
}

export interface PaginationParams {
  page: number;
  limit: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
}

export interface PaginatedResponse<T> {
  data: T[];
  pagination: {
    page: number;
    limit: number;
    total: number;
    totalPages: number;
    hasNext: boolean;
    hasPrev: boolean;
  };
}

export interface PaginatedUsersResponse extends PaginatedResponse<User> {}

export interface APIError {
  error: string;
  message?: string;
  details?: string;
  statusCode?: number;
}

export interface APIResponse<T = any> {
  success: boolean;
  data?: T;
  error?: APIError;
  message?: string;
}

export interface UserFilters {
  userType?: 'A' | 'U';
  role?: UserRole;
  search?: string;
}

export interface UserSearchParams extends PaginationParams {
  filters?: UserFilters;
}

export interface UserStats {
  totalUsers: number;
  adminUsers: number;
  generalUsers: number;
  activeUsers: number;
}

export interface PasswordChangeRequest {
  currentPassword: string;
  newPassword: string;
  confirmPassword: string;
}

export interface UserSession {
  userId: string;
  token: string;
  expiresAt: string;
  refreshToken?: string;
}

export interface RefreshTokenRequest {
  refreshToken: string;
}

export interface RefreshTokenResponse {
  token: string;
  refreshToken: string;
  expiresAt: string;
}

// Utility type for user creation without system fields
export type UserCreateData = Omit<User, 'userId' | 'createdAt' | 'updatedAt'>;

// Utility type for user updates
export type UserUpdateData = Partial<Omit<User, 'userId' | 'createdAt' | 'updatedAt'>>;

// Type guard functions
export const isAdmin = (user: User | AuthUser): boolean => {
  return user.userType === 'A';
};

export const isGeneralUser = (user: User | AuthUser): boolean => {
  return user.userType === 'U';
};

export const getUserRole = (userType: 'A' | 'U'): UserRole => {
  return userType === 'A' ? UserRole.ADMIN : UserRole.GENERAL;
};

export const hasAdminAccess = (user: User | AuthUser): boolean => {
  return isAdmin(user);
};

// Validation constants
export const USER_VALIDATION_RULES = {
  USER_ID_LENGTH: 8,
  FIRST_NAME_MAX_LENGTH: 20,
  LAST_NAME_MAX_LENGTH: 20,
  PASSWORD_LENGTH: 8,
  VALID_USER_TYPES: ['A', 'U'] as const,
} as const;

// Default pagination settings
export const DEFAULT_PAGINATION: PaginationParams = {
  page: 1,
  limit: 10,
  sortBy: 'userId',
  sortOrder: 'asc',
};

export const MAX_PAGINATION_LIMIT = 100;