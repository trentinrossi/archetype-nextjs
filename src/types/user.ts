// User management types based on the API schema

// Base User interface matching UserResponse from API
export interface User {
  userId: string;
  firstName: string;
  lastName: string;
  userType: string; // 'A' for Admin, 'R' for Regular
  fullName: string;
  createdAt: string;
  updatedAt: string;
}

// API Request types
export interface UserCreateRequest {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: string; // 'A' for Admin, 'R' for Regular
}

export interface UserUpdateRequest {
  firstName: string;
  lastName: string;
  password: string;
  userType: string; // 'A' for Admin, 'R' for Regular
}

export interface UserSignOnRequest {
  userId: string;
  password: string;
}

// API Response types
export interface UserResponse {
  userId: string;
  firstName: string;
  lastName: string;
  userType: string;
  fullName: string;
  createdAt: string;
  updatedAt: string;
}

export interface UserSignOnResponse {
  success: boolean;
  message: string;
  userType?: string;
  redirectProgram?: string;
}

export interface UserListResponse {
  users: UserResponse[];
  page: number;
  size: number;
  totalElements: number;
  totalPages: number;
  first: boolean;
  last: boolean;
}

// Pagination types
export interface Pageable {
  page: number;
  size: number;
  sort?: string[];
}

export interface PaginationParams {
  page?: number;
  size?: number;
  sort?: string[];
}

// Form validation types
export interface UserFormData {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  confirmPassword?: string;
  userType: string;
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

export interface SignOnFormData {
  userId: string;
  password: string;
}

export interface SignOnFormErrors {
  userId?: string;
  password?: string;
  general?: string;
}

// API Error types
export interface ApiError {
  message: string;
  code?: string;
  details?: Record<string, any>;
}

export interface ApiErrorResponse {
  error: ApiError;
  timestamp?: string;
  path?: string;
}

// Filter and search types
export interface UserFilters {
  userType?: string;
  search?: string;
  startUserId?: string; // For pagination starting from specific user ID
}

export interface UserSearchParams extends PaginationParams {
  filters?: UserFilters;
}

// Table and UI types
export interface UserTableColumn {
  key: keyof User | 'actions' | 'selection';
  label: string;
  sortable?: boolean;
  width?: string;
}

export interface UserTableRow extends User {
  actions?: React.ReactNode;
  selected?: boolean;
}

// Modal and form types
export interface UserModalProps {
  isOpen: boolean;
  onClose: () => void;
  user?: User;
  mode: 'create' | 'edit' | 'view' | 'delete';
}

export interface UserFormProps {
  user?: User;
  onSubmit: (data: UserFormData) => Promise<void>;
  onCancel: () => void;
  isLoading?: boolean;
  errors?: UserFormErrors;
  mode: 'create' | 'edit';
}

// Service types
export interface UserService {
  getUsers: (params?: Pageable) => Promise<UserListResponse>;
  getUserById: (userId: string) => Promise<UserResponse>;
  createUser: (data: UserCreateRequest) => Promise<UserResponse>;
  updateUser: (userId: string, data: UserUpdateRequest) => Promise<UserResponse>;
  deleteUser: (userId: string) => Promise<void>;
  signIn: (data: UserSignOnRequest) => Promise<UserSignOnResponse>;
}

// Context types
export interface UserContextValue {
  users: User[];
  currentUser: User | null;
  isLoading: boolean;
  error: string | null;
  pagination: UserListResponse | null;
  selectedUser: User | null;
  fetchUsers: (params?: Pageable) => Promise<void>;
  createUser: (data: UserCreateRequest) => Promise<void>;
  updateUser: (userId: string, data: UserUpdateRequest) => Promise<void>;
  deleteUser: (userId: string) => Promise<void>;
  selectUser: (user: User | null) => void;
  clearError: () => void;
}

// Hook types
export interface UseUsersReturn {
  users: User[];
  isLoading: boolean;
  error: string | null;
  pagination: UserListResponse | null;
  refetch: () => Promise<void>;
}

export interface UseUserReturn {
  user: User | null;
  isLoading: boolean;
  error: string | null;
  refetch: () => Promise<void>;
}

// Utility types
export type UserSortField = keyof Pick<User, 'userId' | 'firstName' | 'lastName' | 'userType' | 'createdAt' | 'updatedAt'>;

export type UserTypeOption = {
  value: string;
  label: string;
};

export const USER_TYPE_OPTIONS: UserTypeOption[] = [
  { value: 'A', label: 'Admin' },
  { value: 'R', label: 'Regular' }
];

export type CreateUserData = Omit<User, 'fullName' | 'createdAt' | 'updatedAt'>;

export type UpdateUserData = Partial<Omit<User, 'userId' | 'fullName' | 'createdAt' | 'updatedAt'>>;

export type UserListItem = Pick<User, 'userId' | 'firstName' | 'lastName' | 'userType' | 'fullName'>;

// Navigation and action types
export type UserAction = 'view' | 'edit' | 'delete';

export interface UserActionSelection {
  userId: string;
  action: UserAction;
}

// Message types for user feedback
export interface UserMessage {
  type: 'success' | 'error' | 'info' | 'warning';
  message: string;
  details?: string;
}

// Page state types
export interface UserListPageState {
  currentPage: number;
  pageSize: number;
  searchUserId: string;
  selectedUsers: UserActionSelection[];
  message: UserMessage | null;
}

export interface UserFormPageState {
  isSubmitting: boolean;
  message: UserMessage | null;
  errors: UserFormErrors;
}