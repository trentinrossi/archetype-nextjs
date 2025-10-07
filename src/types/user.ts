// User authentication and management types
export interface User {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: 'A' | 'G'; // A = ADMIN, G = GENERAL
  createdAt?: string;
  updatedAt?: string;
}

export interface UserSecurityDTO {
  userId: string;
  firstName: string;
  lastName: string;
  userType: 'A' | 'G';
  createdAt: string;
  updatedAt: string;
}

export interface SignonRequestDTO {
  userId: string;
  password: string;
}

export interface SignonResponseDTO {
  success: boolean;
  user?: UserSecurityDTO;
  redirectProgram?: string;
  message?: string;
  errorCode?: string;
}

export interface CreateUserSecurityRequest {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: 'A' | 'G';
}

export interface UpdateUserSecurityRequest {
  firstName?: string;
  lastName?: string;
  userType?: 'A' | 'G';
}

export interface ChangePasswordRequest {
  currentPassword: string;
  newPassword: string;
}

export interface UserListResponse {
  users: UserSecurityDTO[];
  totalCount: number;
  currentPage: number;
  totalPages: number;
  pageSize: number;
}

export interface UserListRequest {
  page?: number;
  pageSize?: number;
  filter?: string;
  sortBy?: 'userId' | 'firstName' | 'lastName' | 'userType' | 'createdAt';
  sortOrder?: 'asc' | 'desc';
}

export interface AuthContextType {
  user: UserSecurityDTO | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  login: (credentials: SignonRequestDTO) => Promise<SignonResponseDTO>;
  logout: () => Promise<void>;
  validateSession: () => Promise<boolean>;
}

export interface ApiResponse<T = any> {
  success: boolean;
  data?: T;
  message?: string;
  errorCode?: string;
  errors?: string[];
}

export interface PaginationParams {
  page: number;
  pageSize: number;
}

export interface SortParams {
  sortBy: string;
  sortOrder: 'asc' | 'desc';
}

export interface FilterParams {
  filter?: string;
  userType?: 'A' | 'G';
}

export type UserType = 'A' | 'G';

export const USER_TYPES = {
  ADMIN: 'A' as const,
  GENERAL: 'G' as const,
} as const;

export const USER_TYPE_LABELS = {
  A: 'Administrator',
  G: 'General User',
} as const;

export const REDIRECT_PROGRAMS = {
  ADMIN: 'COUSR00C',
  GENERAL: 'COUSR00C',
} as const;

export const COBOL_PROGRAMS = {
  USER_LIST: 'COUSR00C',
  ADD_USER: 'COUSR01C',
  UPDATE_USER: 'COUSR02C',
  DELETE_USER: 'COUSR03C',
  SIGN_ON: 'COSGN00C',
} as const;

export interface ValidationError {
  field: string;
  message: string;
  code?: string;
}

export interface UserFormData {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  confirmPassword?: string;
  userType: UserType;
}

export interface UserFormErrors {
  userId?: string;
  firstName?: string;
  lastName?: string;
  password?: string;
  confirmPassword?: string;
  userType?: string;
  general?: string;
}

export interface SessionData {
  user: UserSecurityDTO;
  expiresAt: string;
  sessionId: string;
}

export interface AuthError {
  code: string;
  message: string;
  field?: string;
}

export const AUTH_ERROR_CODES = {
  INVALID_CREDENTIALS: 'INVALID_CREDENTIALS',
  USER_NOT_FOUND: 'USER_NOT_FOUND',
  INVALID_PASSWORD: 'INVALID_PASSWORD',
  SESSION_EXPIRED: 'SESSION_EXPIRED',
  UNAUTHORIZED: 'UNAUTHORIZED',
  VALIDATION_ERROR: 'VALIDATION_ERROR',
} as const;

export const USER_VALIDATION_RULES = {
  USER_ID: {
    MIN_LENGTH: 1,
    MAX_LENGTH: 8,
    PATTERN: /^[A-Z0-9]+$/,
  },
  FIRST_NAME: {
    MIN_LENGTH: 1,
    MAX_LENGTH: 20,
    PATTERN: /^[A-Za-z\s]+$/,
  },
  LAST_NAME: {
    MIN_LENGTH: 1,
    MAX_LENGTH: 20,
    PATTERN: /^[A-Za-z\s]+$/,
  },
  PASSWORD: {
    MIN_LENGTH: 1,
    MAX_LENGTH: 8,
    PATTERN: /^[A-Za-z0-9!@#$%^&*]+$/,
  },
} as const;

export const DEFAULT_PAGE_SIZE = 10;
export const MAX_PAGE_SIZE = 100;

export type UserAction = 'create' | 'update' | 'delete' | 'view';

export interface UserPermissions {
  canCreate: boolean;
  canUpdate: boolean;
  canDelete: boolean;
  canView: boolean;
}

export const getUserPermissions = (userType: UserType): UserPermissions => {
  switch (userType) {
    case 'A':
      return {
        canCreate: true,
        canUpdate: true,
        canDelete: true,
        canView: true,
      };
    case 'G':
      return {
        canCreate: false,
        canUpdate: false,
        canDelete: false,
        canView: true,
      };
    default:
      return {
        canCreate: false,
        canUpdate: false,
        canDelete: false,
        canView: false,
      };
  }
};