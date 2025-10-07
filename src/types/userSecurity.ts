// src/types/userSecurity.ts

// Base User Security entity based on USRSEC file structure
export interface UserSecurity {
  id: string; // SEC-USR-ID (8 chars)
  firstName: string; // SEC-USR-FNAME (20 chars)
  lastName: string; // SEC-USR-LNAME (20 chars)
  password: string; // SEC-USR-PWD (8 chars)
  userType: string; // SEC-USR-TYPE (1 char)
  createdAt: string;
  updatedAt: string;
  isActive: boolean;
  lastLoginAt?: string;
}

// DTO for user security data transfer (without sensitive fields)
export interface UserSecurityDTO {
  id: string;
  firstName: string;
  lastName: string;
  userType: string;
  createdAt: string;
  updatedAt: string;
  isActive: boolean;
  lastLoginAt?: string;
  fullName?: string;
}

// Authentication request/response types
export interface SignonRequestDTO {
  userId: string;
  password: string;
  rememberMe?: boolean;
}

export interface SignonResponseDTO {
  success: boolean;
  user?: UserSecurityDTO;
  token?: string;
  refreshToken?: string;
  expiresAt?: string;
  message?: string;
  errors?: ValidationError[];
}

// User management request types
export interface CreateUserSecurityRequest {
  firstName: string;
  lastName: string;
  password: string;
  userType: string;
  isActive?: boolean;
}

export interface UpdateUserSecurityRequest {
  firstName?: string;
  lastName?: string;
  userType?: string;
  isActive?: boolean;
}

export interface ChangePasswordRequest {
  userId: string;
  currentPassword: string;
  newPassword: string;
  confirmPassword: string;
}

// Validation and error handling types
export interface ValidationError {
  field: string;
  message: string;
  code?: string;
}

export interface ValidationResponseDTO {
  isValid: boolean;
  errors: ValidationError[];
  warnings?: ValidationError[];
}

// API response wrapper types
export interface ApiResponse<T = any> {
  success: boolean;
  data?: T;
  message?: string;
  errors?: ValidationError[];
  timestamp: string;
}

export interface PaginatedResponse<T = any> {
  data: T[];
  pagination: PaginationInfo;
  totalCount: number;
}

export interface PaginationInfo {
  page: number;
  pageSize: number;
  totalPages: number;
  hasNext: boolean;
  hasPrevious: boolean;
}

// Query and filter types
export interface UserSecurityQuery {
  page?: number;
  pageSize?: number;
  search?: string;
  userType?: string;
  isActive?: boolean;
  sortBy?: 'firstName' | 'lastName' | 'createdAt' | 'lastLoginAt';
  sortOrder?: 'asc' | 'desc';
}

export interface UserSecurityFilters {
  userTypes?: string[];
  isActive?: boolean;
  createdAfter?: string;
  createdBefore?: string;
  lastLoginAfter?: string;
  lastLoginBefore?: string;
}

// User type enumeration based on SEC-USR-TYPE (1 char)
export enum UserType {
  ADMIN = 'A',
  USER = 'U',
  GUEST = 'G',
  SYSTEM = 'S',
  MANAGER = 'M'
}

// User status enumeration
export enum UserStatus {
  ACTIVE = 'active',
  INACTIVE = 'inactive',
  SUSPENDED = 'suspended',
  PENDING = 'pending'
}

// Authentication status
export enum AuthStatus {
  AUTHENTICATED = 'authenticated',
  UNAUTHENTICATED = 'unauthenticated',
  EXPIRED = 'expired',
  INVALID = 'invalid'
}

// Form state types for UI components
export interface UserSecurityFormData {
  firstName: string;
  lastName: string;
  password: string;
  confirmPassword?: string;
  userType: string;
  isActive: boolean;
}

export interface SignonFormData {
  userId: string;
  password: string;
  rememberMe: boolean;
}

export interface ChangePasswordFormData {
  currentPassword: string;
  newPassword: string;
  confirmPassword: string;
}

// Table column configuration for data display
export interface UserSecurityTableColumn {
  key: keyof UserSecurityDTO | 'actions';
  label: string;
  sortable?: boolean;
  width?: string;
  align?: 'left' | 'center' | 'right';
}

// Bulk operations
export interface BulkUserOperation {
  userIds: string[];
  operation: 'activate' | 'deactivate' | 'delete' | 'changeType';
  newUserType?: string;
}

export interface BulkOperationResult {
  success: boolean;
  processedCount: number;
  failedCount: number;
  errors?: ValidationError[];
}

// Audit and logging types
export interface UserSecurityAuditLog {
  id: string;
  userId: string;
  action: string;
  details: Record<string, any>;
  performedBy: string;
  performedAt: string;
  ipAddress?: string;
  userAgent?: string;
}

// Password policy types
export interface PasswordPolicy {
  minLength: number;
  maxLength: number;
  requireUppercase: boolean;
  requireLowercase: boolean;
  requireNumbers: boolean;
  requireSpecialChars: boolean;
  forbiddenPasswords: string[];
}

export interface PasswordValidationResult {
  isValid: boolean;
  score: number;
  feedback: string[];
  requirements: {
    length: boolean;
    uppercase: boolean;
    lowercase: boolean;
    numbers: boolean;
    specialChars: boolean;
    notForbidden: boolean;
  };
}

// Session management types
export interface UserSession {
  sessionId: string;
  userId: string;
  createdAt: string;
  expiresAt: string;
  isActive: boolean;
  ipAddress?: string;
  userAgent?: string;
  lastActivity: string;
}

// Export all types for easy importing
export type {
  UserSecurity,
  UserSecurityDTO,
  SignonRequestDTO,
  SignonResponseDTO,
  CreateUserSecurityRequest,
  UpdateUserSecurityRequest,
  ChangePasswordRequest,
  ValidationResponseDTO,
  ApiResponse,
  PaginatedResponse,
  PaginationInfo,
  UserSecurityQuery,
  UserSecurityFilters,
  UserSecurityFormData,
  SignonFormData,
  ChangePasswordFormData,
  UserSecurityTableColumn,
  BulkUserOperation,
  BulkOperationResult,
  UserSecurityAuditLog,
  PasswordPolicy,
  PasswordValidationResult,
  UserSession,
  ValidationError
};