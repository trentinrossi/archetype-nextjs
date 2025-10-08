// Base user security entity
export interface UserSecurity {
  userId: string;
  username: string;
  email: string;
  firstName: string;
  lastName: string;
  isActive: boolean;
  createdAt: string;
  updatedAt: string;
  lastLoginAt?: string;
  failedLoginAttempts: number;
  isLocked: boolean;
  lockedUntil?: string;
  passwordExpiresAt?: string;
  mustChangePassword: boolean;
  roles: string[];
  permissions: string[];
}

// Authentication DTOs
export interface SignonRequestDTO {
  username: string;
  password: string;
  rememberMe?: boolean;
}

export interface SignonResponseDTO {
  success: boolean;
  token?: string;
  refreshToken?: string;
  user?: UserSecurity;
  message?: string;
  requiresPasswordChange?: boolean;
  expiresAt?: string;
}

export interface ValidateAuthRequest {
  token: string;
}

export interface ValidateAuthResponse {
  valid: boolean;
  user?: UserSecurity;
  message?: string;
}

export interface ExitRequest {
  token?: string;
  userId?: string;
}

export interface ExitResponse {
  success: boolean;
  message?: string;
}

export interface InvalidKeyRequest {
  username?: string;
  attemptedKey?: string;
  timestamp: string;
}

export interface InvalidKeyResponse {
  success: boolean;
  message?: string;
  lockoutDuration?: number;
  remainingAttempts?: number;
}

// User management DTOs
export interface CreateUserSecurityRequest {
  username: string;
  email: string;
  firstName: string;
  lastName: string;
  password: string;
  confirmPassword: string;
  roles: string[];
  permissions?: string[];
  isActive?: boolean;
  mustChangePassword?: boolean;
  passwordExpiresAt?: string;
}

export interface UpdateUserSecurityRequest {
  email?: string;
  firstName?: string;
  lastName?: string;
  roles?: string[];
  permissions?: string[];
  isActive?: boolean;
  mustChangePassword?: boolean;
  passwordExpiresAt?: string;
}

export interface ChangePasswordRequest {
  currentPassword: string;
  newPassword: string;
  confirmNewPassword: string;
}

export interface ChangePasswordResponse {
  success: boolean;
  message?: string;
  requiresReauth?: boolean;
}

// Pagination and filtering
export interface GetUsersRequest {
  page?: number;
  limit?: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
  search?: string;
  isActive?: boolean;
  roles?: string[];
  createdAfter?: string;
  createdBefore?: string;
}

export interface GetUsersResponse {
  users: UserSecurity[];
  pagination: {
    page: number;
    limit: number;
    total: number;
    totalPages: number;
    hasNext: boolean;
    hasPrev: boolean;
  };
}

export interface GetUserByIdResponse {
  user: UserSecurity;
}

// User activation/deactivation
export interface ActivateUserRequest {
  reason?: string;
}

export interface ActivateUserResponse {
  success: boolean;
  message?: string;
  user?: UserSecurity;
}

export interface DeactivateUserRequest {
  reason?: string;
}

export interface DeactivateUserResponse {
  success: boolean;
  message?: string;
  user?: UserSecurity;
}

// Generic API response wrapper
export interface ApiResponse<T = any> {
  success: boolean;
  data?: T;
  message?: string;
  errors?: string[];
  timestamp: string;
}

// Error types
export interface ApiError {
  code: string;
  message: string;
  field?: string;
  details?: Record<string, any>;
}

export interface ValidationError extends ApiError {
  field: string;
  value?: any;
  constraints?: string[];
}

// Authentication context types
export interface AuthState {
  isAuthenticated: boolean;
  user: UserSecurity | null;
  token: string | null;
  refreshToken: string | null;
  isLoading: boolean;
  error: string | null;
}

export interface AuthContextType extends AuthState {
  signon: (credentials: SignonRequestDTO) => Promise<SignonResponseDTO>;
  signout: () => Promise<void>;
  validateAuth: () => Promise<boolean>;
  changePassword: (request: ChangePasswordRequest) => Promise<ChangePasswordResponse>;
  refreshAuthToken: () => Promise<boolean>;
  clearError: () => void;
}

// Form validation types
export interface FormErrors {
  [key: string]: string | undefined;
}

export interface SignonFormData {
  username: string;
  password: string;
  rememberMe: boolean;
}

export interface CreateUserFormData {
  username: string;
  email: string;
  firstName: string;
  lastName: string;
  password: string;
  confirmPassword: string;
  roles: string[];
  isActive: boolean;
  mustChangePassword: boolean;
}

export interface UpdateUserFormData {
  email: string;
  firstName: string;
  lastName: string;
  roles: string[];
  isActive: boolean;
  mustChangePassword: boolean;
}

export interface ChangePasswordFormData {
  currentPassword: string;
  newPassword: string;
  confirmNewPassword: string;
}

// Table and UI types
export interface UserTableColumn {
  key: keyof UserSecurity | 'actions';
  label: string;
  sortable?: boolean;
  width?: string;
  align?: 'left' | 'center' | 'right';
}

export interface UserTableProps {
  users: UserSecurity[];
  loading?: boolean;
  onEdit?: (user: UserSecurity) => void;
  onDelete?: (userId: string) => void;
  onActivate?: (userId: string) => void;
  onDeactivate?: (userId: string) => void;
  onChangePassword?: (userId: string) => void;
}

export interface UserFilters {
  search: string;
  isActive: boolean | null;
  roles: string[];
  dateRange: {
    start: string | null;
    end: string | null;
  };
}

// Constants and enums
export enum UserStatus {
  ACTIVE = 'ACTIVE',
  INACTIVE = 'INACTIVE',
  LOCKED = 'LOCKED',
  PENDING = 'PENDING'
}

export enum UserRole {
  ADMIN = 'ADMIN',
  USER = 'USER',
  MANAGER = 'MANAGER',
  VIEWER = 'VIEWER'
}

export enum Permission {
  READ_USERS = 'READ_USERS',
  CREATE_USERS = 'CREATE_USERS',
  UPDATE_USERS = 'UPDATE_USERS',
  DELETE_USERS = 'DELETE_USERS',
  MANAGE_ROLES = 'MANAGE_ROLES',
  MANAGE_PERMISSIONS = 'MANAGE_PERMISSIONS'
}

// Business logic types for COSGN00C
export interface COSGN00CRequest {
  functionKey: string;
  username?: string;
  password?: string;
  action: 'SIGNON' | 'EXIT' | 'INVALID_KEY' | 'VALIDATE';
}

export interface COSGN00CResponse {
  returnCode: string;
  message: string;
  user?: UserSecurity;
  token?: string;
  nextAction?: string;
}

// Business logic types for COUSR00C, COUSR01C, COUSR02C, COUSR03C
export interface COUSR00CRequest {
  action: 'LIST' | 'SEARCH';
  searchCriteria?: {
    username?: string;
    email?: string;
    isActive?: boolean;
    roles?: string[];
  };
  pagination?: {
    page: number;
    limit: number;
  };
}

export interface COUSR01CRequest {
  action: 'CREATE';
  userData: CreateUserSecurityRequest;
}

export interface COUSR02CRequest {
  action: 'UPDATE';
  userId: string;
  userData: UpdateUserSecurityRequest;
}

export interface COUSR03CRequest {
  action: 'DELETE' | 'ACTIVATE' | 'DEACTIVATE' | 'CHANGE_PASSWORD';
  userId: string;
  passwordData?: ChangePasswordRequest;
  reason?: string;
}

export interface COUSRResponse {
  returnCode: string;
  message: string;
  data?: any;
  errors?: ValidationError[];
}

// Hook types
export interface UseUserSecurityReturn {
  users: UserSecurity[];
  currentUser: UserSecurity | null;
  loading: boolean;
  error: string | null;
  pagination: GetUsersResponse['pagination'] | null;
  fetchUsers: (params?: GetUsersRequest) => Promise<void>;
  fetchUserById: (userId: string) => Promise<UserSecurity | null>;
  createUser: (userData: CreateUserSecurityRequest) => Promise<boolean>;
  updateUser: (userId: string, userData: UpdateUserSecurityRequest) => Promise<boolean>;
  deleteUser: (userId: string) => Promise<boolean>;
  activateUser: (userId: string, reason?: string) => Promise<boolean>;
  deactivateUser: (userId: string, reason?: string) => Promise<boolean>;
  changeUserPassword: (userId: string, passwordData: ChangePasswordRequest) => Promise<boolean>;
  clearError: () => void;
}

// Service response types
export interface ServiceResponse<T = any> {
  success: boolean;
  data?: T;
  error?: string;
  statusCode?: number;
}

export type UserSecurityServiceResponse<T = any> = Promise<ServiceResponse<T>>;