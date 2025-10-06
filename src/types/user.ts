// src/types/user.ts

// Core User entity based on USRSEC file structure
export interface User {
  userId: string;        // 8 characters
  firstName: string;     // 20 characters
  lastName: string;      // 20 characters
  password: string;      // 8 characters
  userType: string;      // 1 character
  createdAt?: string;
  updatedAt?: string;
}

// User types enum for validation
export enum UserType {
  ADMIN = 'A',
  USER = 'U',
  GUEST = 'G',
  MANAGER = 'M',
  OPERATOR = 'O'
}

// API Request types
export interface CreateUserRequest {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: UserType;
}

export interface UpdateUserRequest {
  firstName?: string;
  lastName?: string;
  password?: string;
  userType?: UserType;
}

export interface UserLoginRequest {
  userId: string;
  password: string;
}

export interface ChangePasswordRequest {
  userId: string;
  currentPassword: string;
  newPassword: string;
}

// API Response types
export interface UserResponse {
  userId: string;
  firstName: string;
  lastName: string;
  userType: UserType;
  createdAt: string;
  updatedAt: string;
}

export interface UserListResponse {
  users: UserResponse[];
  pagination: PaginationInfo;
}

export interface UserLoginResponse {
  user: UserResponse;
  token: string;
  expiresAt: string;
}

export interface UserCreateResponse {
  user: UserResponse;
  message: string;
}

export interface UserUpdateResponse {
  user: UserResponse;
  message: string;
}

export interface UserDeleteResponse {
  message: string;
  deletedUserId: string;
}

// Pagination types
export interface PaginationInfo {
  currentPage: number;
  totalPages: number;
  totalItems: number;
  itemsPerPage: number;
  hasNextPage: boolean;
  hasPreviousPage: boolean;
}

export interface PaginationRequest {
  page?: number;
  limit?: number;
  sortBy?: keyof User;
  sortOrder?: 'asc' | 'desc';
}

// Search and filter types
export interface UserSearchRequest extends PaginationRequest {
  userId?: string;
  firstName?: string;
  lastName?: string;
  userType?: UserType;
  searchTerm?: string;
}

export interface UserFilterOptions {
  userTypes: UserType[];
  createdDateRange?: {
    startDate: string;
    endDate: string;
  };
}

// Form validation types
export interface UserFormData {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  confirmPassword: string;
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

export interface LoginFormData {
  userId: string;
  password: string;
}

export interface LoginFormErrors {
  userId?: string;
  password?: string;
  general?: string;
}

export interface ChangePasswordFormData {
  currentPassword: string;
  newPassword: string;
  confirmNewPassword: string;
}

export interface ChangePasswordFormErrors {
  currentPassword?: string;
  newPassword?: string;
  confirmNewPassword?: string;
  general?: string;
}

// Validation rules based on COBOL field constraints
export interface UserValidationRules {
  userId: {
    minLength: 1;
    maxLength: 8;
    pattern: RegExp;
    required: true;
  };
  firstName: {
    minLength: 1;
    maxLength: 20;
    required: true;
  };
  lastName: {
    minLength: 1;
    maxLength: 20;
    required: true;
  };
  password: {
    minLength: 8;
    maxLength: 8;
    pattern: RegExp;
    required: true;
  };
  userType: {
    allowedValues: UserType[];
    required: true;
  };
}

// API Error types
export interface APIError {
  message: string;
  code: string;
  field?: string;
  details?: string[];
}

export interface ValidationError {
  field: string;
  message: string;
  code: string;
}

export interface APIErrorResponse {
  error: APIError;
  timestamp: string;
  path: string;
  status: number;
}

// Table and UI types
export interface UserTableColumn {
  key: keyof UserResponse | 'actions';
  label: string;
  sortable: boolean;
  width?: string;
}

export interface UserTableRow extends UserResponse {
  actions?: React.ReactNode;
}

export interface UserTableProps {
  users: UserResponse[];
  loading?: boolean;
  error?: string;
  onEdit?: (user: UserResponse) => void;
  onDelete?: (userId: string) => void;
  onView?: (user: UserResponse) => void;
  pagination?: PaginationInfo;
  onPageChange?: (page: number) => void;
  onSort?: (column: keyof UserResponse, order: 'asc' | 'desc') => void;
}

// Modal and form types
export interface UserModalProps {
  isOpen: boolean;
  onClose: () => void;
  user?: UserResponse;
  mode: 'create' | 'edit' | 'view';
  onSubmit?: (userData: UserFormData) => Promise<void>;
}

export interface UserFormProps {
  initialData?: Partial<UserFormData>;
  onSubmit: (data: UserFormData) => Promise<void>;
  loading?: boolean;
  errors?: UserFormErrors;
  mode: 'create' | 'edit';
}

// Authentication and session types
export interface UserSession {
  user: UserResponse;
  token: string;
  expiresAt: string;
  isAuthenticated: boolean;
}

export interface AuthContextType {
  session: UserSession | null;
  login: (credentials: UserLoginRequest) => Promise<void>;
  logout: () => void;
  updateUser: (userData: Partial<UserResponse>) => void;
  isLoading: boolean;
  error: string | null;
}

// Hook return types
export interface UseUsersReturn {
  users: UserResponse[];
  loading: boolean;
  error: string | null;
  pagination: PaginationInfo | null;
  fetchUsers: (params?: UserSearchRequest) => Promise<void>;
  createUser: (userData: CreateUserRequest) => Promise<void>;
  updateUser: (userId: string, userData: UpdateUserRequest) => Promise<void>;
  deleteUser: (userId: string) => Promise<void>;
  refreshUsers: () => Promise<void>;
}

export interface UseUserReturn {
  user: UserResponse | null;
  loading: boolean;
  error: string | null;
  fetchUser: (userId: string) => Promise<void>;
  updateUser: (userData: UpdateUserRequest) => Promise<void>;
  deleteUser: () => Promise<void>;
}

// Constants for validation
export const USER_VALIDATION_RULES: UserValidationRules = {
  userId: {
    minLength: 1,
    maxLength: 8,
    pattern: /^[A-Z0-9]{1,8}$/,
    required: true,
  },
  firstName: {
    minLength: 1,
    maxLength: 20,
    required: true,
  },
  lastName: {
    minLength: 1,
    maxLength: 20,
    required: true,
  },
  password: {
    minLength: 8,
    maxLength: 8,
    pattern: /^(?=.*[A-Z])(?=.*[a-z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8}$/,
    required: true,
  },
  userType: {
    allowedValues: Object.values(UserType),
    required: true,
  },
};

// Default pagination settings
export const DEFAULT_PAGINATION: PaginationRequest = {
  page: 0,
  limit: 10,
  sortBy: 'userId',
  sortOrder: 'asc',
};

// User type display labels
export const USER_TYPE_LABELS: Record<UserType, string> = {
  [UserType.ADMIN]: 'Administrator',
  [UserType.USER]: 'User',
  [UserType.GUEST]: 'Guest',
  [UserType.MANAGER]: 'Manager',
  [UserType.OPERATOR]: 'Operator',
};