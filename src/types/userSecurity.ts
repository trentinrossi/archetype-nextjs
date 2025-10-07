// src/types/userSecurity.ts

export interface SignonRequestDTO {
  userId: string;
  password: string;
  systemId?: string;
  workstationId?: string;
}

export interface UserSecurityDTO {
  userId: string;
  userName: string;
  userType: 'ADMIN' | 'GENERAL';
  email: string;
  firstName: string;
  lastName: string;
  isActive: boolean;
  lastSignonDate?: Date;
  passwordExpiryDate?: Date;
  failedAttempts: number;
  isLocked: boolean;
  createdDate: Date;
  modifiedDate: Date;
  createdBy: string;
  modifiedBy: string;
}

export interface CreateUserSecurityRequest {
  userId: string;
  userName: string;
  userType: 'ADMIN' | 'GENERAL';
  email: string;
  firstName: string;
  lastName: string;
  password: string;
  confirmPassword: string;
  isActive?: boolean;
}

export interface UpdateUserSecurityRequest {
  userName?: string;
  userType?: 'ADMIN' | 'GENERAL';
  email?: string;
  firstName?: string;
  lastName?: string;
  isActive?: boolean;
}

export interface ChangePasswordRequest {
  userId: string;
  currentPassword: string;
  newPassword: string;
  confirmPassword: string;
}

export interface ValidationResponseDTO {
  isValid: boolean;
  errorCode?: string;
  errorMessage?: string;
  validationErrors?: ValidationError[];
}

export interface ValidationError {
  field: string;
  message: string;
  code: string;
}

export interface SignonResponseDTO {
  success: boolean;
  userId?: string;
  userName?: string;
  userType?: 'ADMIN' | 'GENERAL';
  sessionToken?: string;
  expiresAt?: Date;
  errorCode?: string;
  errorMessage?: string;
  remainingAttempts?: number;
}

export interface UserSecurityListResponse {
  users: UserSecurityDTO[];
  totalCount: number;
  page: number;
  pageSize: number;
  totalPages: number;
}

export interface UserSecurityApiResponse<T = any> {
  success: boolean;
  data?: T;
  error?: {
    code: string;
    message: string;
    details?: string;
  };
  timestamp: string;
}

export interface UserSecurityFilters {
  userType?: 'ADMIN' | 'GENERAL';
  isActive?: boolean;
  isLocked?: boolean;
  searchTerm?: string;
  page?: number;
  pageSize?: number;
  sortBy?: keyof UserSecurityDTO;
  sortOrder?: 'asc' | 'desc';
}

export interface PasswordPolicy {
  minLength: number;
  maxLength: number;
  requireUppercase: boolean;
  requireLowercase: boolean;
  requireNumbers: boolean;
  requireSpecialChars: boolean;
  expiryDays: number;
  maxFailedAttempts: number;
}

export interface UserSecurityAuditLog {
  id: string;
  userId: string;
  action: 'CREATE' | 'UPDATE' | 'DELETE' | 'SIGNON' | 'SIGNOFF' | 'PASSWORD_CHANGE' | 'ACTIVATE' | 'DEACTIVATE' | 'LOCK' | 'UNLOCK';
  timestamp: Date;
  performedBy: string;
  details?: string;
  ipAddress?: string;
  userAgent?: string;
}

export interface SessionInfo {
  sessionToken: string;
  userId: string;
  userName: string;
  userType: 'ADMIN' | 'GENERAL';
  isActive: boolean;
  createdAt: Date;
  expiresAt: Date;
  lastActivity: Date;
}

export type UserSecurityFormData = Omit<CreateUserSecurityRequest, 'confirmPassword'> & {
  confirmPassword?: string;
};

export type UserSecurityUpdateFormData = UpdateUserSecurityRequest;

export type PasswordChangeFormData = Omit<ChangePasswordRequest, 'userId'>;

export interface UserSecurityTableColumn {
  key: keyof UserSecurityDTO;
  label: string;
  sortable: boolean;
  width?: string;
  align?: 'left' | 'center' | 'right';
}

export interface UserSecurityActionResult {
  success: boolean;
  message: string;
  data?: UserSecurityDTO;
  errors?: ValidationError[];
}

export const USER_TYPES = {
  ADMIN: 'ADMIN',
  GENERAL: 'GENERAL'
} as const;

export const USER_ACTIONS = {
  CREATE: 'CREATE',
  UPDATE: 'UPDATE',
  DELETE: 'DELETE',
  SIGNON: 'SIGNON',
  SIGNOFF: 'SIGNOFF',
  PASSWORD_CHANGE: 'PASSWORD_CHANGE',
  ACTIVATE: 'ACTIVATE',
  DEACTIVATE: 'DEACTIVATE',
  LOCK: 'LOCK',
  UNLOCK: 'UNLOCK'
} as const;

export const ERROR_CODES = {
  INVALID_CREDENTIALS: 'INVALID_CREDENTIALS',
  USER_LOCKED: 'USER_LOCKED',
  USER_INACTIVE: 'USER_INACTIVE',
  PASSWORD_EXPIRED: 'PASSWORD_EXPIRED',
  USER_NOT_FOUND: 'USER_NOT_FOUND',
  USER_ALREADY_EXISTS: 'USER_ALREADY_EXISTS',
  INVALID_PASSWORD_FORMAT: 'INVALID_PASSWORD_FORMAT',
  PASSWORD_MISMATCH: 'PASSWORD_MISMATCH',
  INSUFFICIENT_PRIVILEGES: 'INSUFFICIENT_PRIVILEGES',
  SESSION_EXPIRED: 'SESSION_EXPIRED',
  VALIDATION_ERROR: 'VALIDATION_ERROR',
  SYSTEM_ERROR: 'SYSTEM_ERROR'
} as const;

export type UserType = typeof USER_TYPES[keyof typeof USER_TYPES];
export type UserAction = typeof USER_ACTIONS[keyof typeof USER_ACTIONS];
export type ErrorCode = typeof ERROR_CODES[keyof typeof ERROR_CODES];