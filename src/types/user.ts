// User Management Types based on COBOL business rules

export interface UserSecurityRecord {
  userId: string;          // SEC-USR-ID (8 chars)
  firstName: string;       // SEC-USR-FNAME (20 chars)
  lastName: string;        // SEC-USR-LNAME (20 chars)
  password: string;        // SEC-USR-PWD (8 chars)
  userType: 'A' | 'U';    // SEC-USR-TYPE (1 char: A=Admin, U=User)
}

export interface UserSecurityDTO {
  userId: string;
  password: string;
  userType: 'ADMIN' | 'GENERAL';
  programName: string;
  transactionId: string;
  active: boolean;
  createdAt?: string;
  updatedAt?: string;
  userTypeDisplayName: string;
  redirectProgram: string;
  canAuthenticate: boolean;
}

export interface SignonRequestDTO {
  userId: string;    // Max 8 characters
  password: string;  // Max 8 characters
}

export interface SignonResponseDTO {
  success: boolean;
  message: string;
  userType: 'ADMIN' | 'GENERAL';
  redirectProgram: string;
}

export interface CreateUserSecurityRequest {
  userId: string;        // Max 8 chars, required
  password: string;      // Max 8 chars, required
  userType: 'ADMIN' | 'GENERAL';  // Required
  programName?: string;  // Optional, default: COSGN00C
  transactionId?: string; // Optional, default: CC00
  active?: boolean;      // Optional, default: true
}

export interface UpdateUserSecurityRequest {
  password?: string;     // Max 8 chars, optional
  userType?: 'ADMIN' | 'GENERAL'; // Optional
  programName?: string;  // Optional
  transactionId?: string; // Optional
  active?: boolean;      // Optional
}

export interface ChangePasswordRequest {
  newPassword: string;   // Max 8 chars, required
}

export interface ValidationResponseDTO {
  valid: boolean;
  message: string;
}

// User List Management Types (COUSR00C)
export interface UserListItem {
  userId: string;
  firstName: string;
  lastName: string;
  userType: string;
}

export interface UserListResponse {
  users: UserListItem[];
  currentPage: number;
  totalPages: number;
  hasNextPage: boolean;
  hasPreviousPage: boolean;
  firstUserId?: string;
  lastUserId?: string;
}

export interface UserListRequest {
  startFromUserId?: string;
  pageSize?: number;
  direction?: 'forward' | 'backward';
}

// User Add Types (COUSR01C)
export interface AddUserRequest {
  firstName: string;     // Required, max 20 chars
  lastName: string;      // Required, max 20 chars
  userId: string;        // Required, max 8 chars
  password: string;      // Required, max 8 chars
  userType: string;      // Required, 1 char
}

export interface AddUserResponse {
  success: boolean;
  message: string;
  userId?: string;
}

// User Update Types (COUSR02C)
export interface UpdateUserRequest {
  userId: string;        // Required for lookup
  firstName: string;     // Required, max 20 chars
  lastName: string;      // Required, max 20 chars
  password: string;      // Required, max 8 chars
  userType: string;      // Required, 1 char
}

export interface UpdateUserResponse {
  success: boolean;
  message: string;
  userId?: string;
}

// User Delete Types (COUSR03C)
export interface DeleteUserRequest {
  userId: string;        // Required, max 8 chars
}

export interface DeleteUserResponse {
  success: boolean;
  message: string;
  userId?: string;
}

export interface UserDetailsResponse {
  userId: string;
  firstName: string;
  lastName: string;
  userType: string;
}

// Common types for all operations
export interface ApiResponse<T = any> {
  success: boolean;
  message: string;
  data?: T;
  error?: string;
}

export interface PaginationParams {
  page?: number;
  size?: number;
  sort?: string;
}

// Screen state management types
export interface ScreenState {
  currentPage: number;
  message: string;
  messageType: 'success' | 'error' | 'info';
  loading: boolean;
  cursorField?: string;
}

// Communication area equivalent for state management
export interface ProgramContext {
  fromProgram: string;
  fromTransaction: string;
  toProgram: string;
  programReenter: boolean;
  selectedUserId?: string;
  pageNumber: number;
  firstUserId?: string;
  lastUserId?: string;
  hasNextPage: boolean;
}