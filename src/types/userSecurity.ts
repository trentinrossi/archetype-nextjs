// src/types/userSecurity.ts

export interface SignonRequestDTO {
  userId: string;
  password: string;
}

export interface UserSecurityDTO {
  userId: string;
  password: string;
  userType: 'ADMIN' | 'GENERAL';
  programName: string;
  transactionId: string;
  active: boolean;
  createdAt: string;
  updatedAt: string;
  userTypeDisplayName: string;
  redirectProgram: string;
  canAuthenticate: boolean;
}

export interface CreateUserSecurityRequest {
  userId: string;
  password: string;
  userType: 'ADMIN' | 'GENERAL';
  programName: string;
  transactionId: string;
  active: boolean;
}

export interface UpdateUserSecurityRequest {
  password?: string;
  userType?: 'ADMIN' | 'GENERAL';
  programName?: string;
  transactionId?: string;
  active?: boolean;
}

export interface ChangePasswordRequest {
  newPassword: string;
}

export interface ValidationResponseDTO {
  valid: boolean;
  message: string;
}

export interface SignonResponseDTO {
  success: boolean;
  user?: UserSecurityDTO;
  message?: string;
  redirectProgram?: string;
}

export interface ApiResponse<T> {
  data: T;
  message: string;
  success: boolean;
  timestamp: string;
}

export interface ApiError {
  message: string;
  code: string;
  details?: Record<string, any>;
  timestamp: string;
}

export interface PaginatedResponse<T> {
  data: T[];
  pagination: {
    page: number;
    limit: number;
    total: number;
    totalPages: number;
    hasNext: boolean;
    hasPrevious: boolean;
  };
}

export interface UserListParams {
  page?: number;
  limit?: number;
  search?: string;
  userType?: 'ADMIN' | 'GENERAL';
  active?: boolean;
  programName?: string;
  sortBy?: 'userId' | 'userType' | 'programName' | 'createdAt' | 'updatedAt';
  sortOrder?: 'asc' | 'desc';
}

export interface UserSecurityFilters {
  userType?: 'ADMIN' | 'GENERAL' | 'ALL';
  active?: boolean | 'ALL';
  programName?: string;
  transactionId?: string;
  canAuthenticate?: boolean | 'ALL';
  dateRange?: {
    startDate: string;
    endDate: string;
  };
}

export interface UserSecuritySummary {
  totalUsers: number;
  activeUsers: number;
  inactiveUsers: number;
  adminUsers: number;
  generalUsers: number;
  authenticatableUsers: number;
}

export interface BulkUserOperation {
  userIds: string[];
  operation: 'activate' | 'deactivate' | 'delete';
  reason?: string;
}

export interface BulkOperationResult {
  successful: string[];
  failed: Array<{
    userId: string;
    error: string;
  }>;
  summary: {
    total: number;
    successful: number;
    failed: number;
  };
}

export interface UserSecurityHistory {
  id: string;
  userId: string;
  action: UserSecurityAction;
  previousValues?: Partial<UserSecurityDTO>;
  newValues?: Partial<UserSecurityDTO>;
  performedBy: string;
  performedAt: string;
  reason?: string;
}

export interface PasswordPolicy {
  minLength: number;
  maxLength: number;
  requireUppercase: boolean;
  requireLowercase: boolean;
  requireNumbers: boolean;
  requireSpecialChars: boolean;
  forbiddenPatterns: string[];
}

export interface UserSecurityValidation {
  userId: ValidationResult;
  password: ValidationResult;
  userType: ValidationResult;
  programName: ValidationResult;
  transactionId: ValidationResult;
  overall: ValidationResult;
}

export interface ValidationResult {
  valid: boolean;
  message?: string;
  warnings?: string[];
}

export interface UserSecurityExport {
  format: 'csv' | 'excel' | 'json';
  filters?: UserSecurityFilters;
  fields: Array<keyof UserSecurityDTO>;
  includeInactive?: boolean;
}

export interface UserSecurityImport {
  file: File;
  format: 'csv' | 'excel' | 'json';
  options: {
    skipDuplicates: boolean;
    updateExisting: boolean;
    validateOnly: boolean;
  };
}

export interface ImportResult {
  totalRecords: number;
  successfulImports: number;
  failedImports: number;
  duplicates: number;
  errors: Array<{
    row: number;
    userId?: string;
    error: string;
  }>;
  warnings: Array<{
    row: number;
    userId?: string;
    warning: string;
  }>;
}

export type UserSecurityAction = 
  | 'create'
  | 'update'
  | 'delete'
  | 'activate'
  | 'deactivate'
  | 'changePassword'
  | 'resetPassword'
  | 'bulkActivate'
  | 'bulkDeactivate'
  | 'bulkDelete'
  | 'import'
  | 'export';

export type UserSecurityStatus = 'active' | 'inactive';

export type UserType = 'ADMIN' | 'GENERAL';

export type SortField = 'userId' | 'userType' | 'programName' | 'transactionId' | 'active' | 'createdAt' | 'updatedAt';

export type SortOrder = 'asc' | 'desc';

export interface UserSecurityState {
  users: UserSecurityDTO[];
  currentUser: UserSecurityDTO | null;
  loading: boolean;
  error: string | null;
  filters: UserSecurityFilters;
  pagination: {
    page: number;
    limit: number;
    total: number;
    totalPages: number;
  };
  selectedUsers: string[];
  summary: UserSecuritySummary | null;
}

export interface AuthState {
  isAuthenticated: boolean;
  user: UserSecurityDTO | null;
  loading: boolean;
  error: string | null;
  sessionExpiry: string | null;
}

export interface UserSecurityFormData extends Omit<CreateUserSecurityRequest, 'password'> {
  password: string;
  confirmPassword: string;
}

export interface UserSecurityUpdateFormData extends UpdateUserSecurityRequest {
  confirmPassword?: string;
}

export interface ChangePasswordFormData extends ChangePasswordRequest {
  confirmPassword: string;
}

export interface UserSecurityTableColumn {
  key: keyof UserSecurityDTO | 'actions';
  label: string;
  sortable: boolean;
  width?: string;
  align?: 'left' | 'center' | 'right';
  render?: (value: any, user: UserSecurityDTO) => React.ReactNode;
}

export interface UserSecurityModalProps {
  isOpen: boolean;
  onClose: () => void;
  user?: UserSecurityDTO;
  mode: 'create' | 'edit' | 'view' | 'delete';
}

export interface UserSecuritySearchParams {
  query: string;
  fields: Array<keyof UserSecurityDTO>;
  caseSensitive: boolean;
  exactMatch: boolean;
}

export interface UserSecurityConfig {
  passwordPolicy: PasswordPolicy;
  sessionTimeout: number;
  maxLoginAttempts: number;
  lockoutDuration: number;
  defaultUserType: UserType;
  defaultProgramName: string;
  allowSelfRegistration: boolean;
  requireEmailVerification: boolean;
}