import {
  User,
  UserFormData,
  UserFormErrors,
  SignOnFormData,
  SignOnFormErrors,
  UserFilters,
  UserSearchParams,
  Pageable,
  ApiError,
  ApiErrorResponse,
  UserSortField,
  CreateUserData,
  UpdateUserData,
  UserListItem
} from '@/types/user';

// User ID validation utility
export const validateUserId = (userId: string): boolean => {
  return userId && userId.trim().length > 0 && userId.length <= 8;
};

// Name validation utility
export const validateName = (name: string, fieldName: string = 'Name'): string | null => {
  if (!name || name.trim().length === 0) {
    return `${fieldName} can NOT be empty...`;
  }
  
  if (name.trim().length > 20) {
    return `${fieldName} cannot exceed 20 characters`;
  }
  
  return null;
};

// Password validation utility
export const validatePassword = (password: string): string | null => {
  if (!password || password.trim().length === 0) {
    return 'Password can NOT be empty...';
  }
  
  if (password.length > 8) {
    return 'Password cannot exceed 8 characters';
  }
  
  return null;
};

// User Type validation utility
export const validateUserType = (userType: string): string | null => {
  if (!userType || userType.trim().length === 0) {
    return 'User Type can NOT be empty...';
  }
  
  if (!['A', 'R'].includes(userType)) {
    return 'User Type must be A (Admin) or R (Regular)';
  }
  
  return null;
};

// User form validation
export const validateUserForm = (data: UserFormData): UserFormErrors => {
  const errors: UserFormErrors = {};
  
  // User ID validation
  if (!data.userId || data.userId.trim().length === 0) {
    errors.userId = 'User ID can NOT be empty...';
  } else if (data.userId.length > 8) {
    errors.userId = 'User ID cannot exceed 8 characters';
  }
  
  // First name validation
  const firstNameError = validateName(data.firstName, 'First Name');
  if (firstNameError) {
    errors.firstName = firstNameError;
  }
  
  // Last name validation
  const lastNameError = validateName(data.lastName, 'Last Name');
  if (lastNameError) {
    errors.lastName = lastNameError;
  }
  
  // Password validation
  const passwordError = validatePassword(data.password);
  if (passwordError) {
    errors.password = passwordError;
  }
  
  // Confirm password validation
  if (data.confirmPassword && data.confirmPassword !== data.password) {
    errors.confirmPassword = 'Passwords do not match';
  }
  
  // User Type validation
  const userTypeError = validateUserType(data.userType);
  if (userTypeError) {
    errors.userType = userTypeError;
  }
  
  return errors;
};

// Sign-on form validation
export const validateSignOnForm = (data: SignOnFormData): SignOnFormErrors => {
  const errors: SignOnFormErrors = {};
  
  // User ID validation
  if (!data.userId || data.userId.trim().length === 0) {
    errors.userId = 'User ID can NOT be empty...';
  }
  
  // Password validation
  if (!data.password || data.password.length === 0) {
    errors.password = 'Password can NOT be empty...';
  }
  
  return errors;
};

// User formatting utilities
export const formatUserFullName = (user: User | UserListItem): string => {
  if (user.fullName) {
    return user.fullName;
  }
  
  if (!user.firstName && !user.lastName) {
    return 'Unknown User';
  }
  
  return `${user.firstName || ''} ${user.lastName || ''}`.trim();
};

export const formatUserDisplayName = (user: User | UserListItem): string => {
  const fullName = formatUserFullName(user);
  return fullName !== 'Unknown User' ? fullName : user.userId;
};

export const formatUserType = (userType: string): string => {
  switch (userType) {
    case 'A':
      return 'Admin';
    case 'R':
      return 'Regular';
    default:
      return userType;
  }
};

export const getUserTypeLabel = (userType: string): string => {
  switch (userType) {
    case 'A':
      return 'A - Admin';
    case 'R':
      return 'R - Regular';
    default:
      return userType;
  }
};

// Date formatting utilities
export const formatDate = (dateString: string): string => {
  try {
    const date = new Date(dateString);
    return date.toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric'
    });
  } catch {
    return 'Invalid Date';
  }
};

export const formatDateTime = (dateString: string): string => {
  try {
    const date = new Date(dateString);
    return date.toLocaleString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    });
  } catch {
    return 'Invalid Date';
  }
};

// Error handling utilities
export const parseApiError = (error: unknown): string => {
  if (typeof error === 'string') {
    return error;
  }
  
  if (error instanceof Error) {
    return error.message;
  }
  
  if (error && typeof error === 'object' && 'message' in error) {
    return String(error.message);
  }
  
  return 'An unexpected error occurred';
};

export const formatApiError = (errorResponse: ApiErrorResponse): string => {
  return errorResponse.error.message || 'An error occurred';
};

export const getErrorMessage = (error: unknown, defaultMessage: string = 'An error occurred'): string => {
  const parsedError = parseApiError(error);
  return parsedError || defaultMessage;
};

// User selection and filtering utilities
export const filterUsers = (users: User[], filters: UserFilters): User[] => {
  return users.filter(user => {
    // User Type filter
    if (filters.userType && user.userType !== filters.userType) {
      return false;
    }
    
    // Search filter (by User ID)
    if (filters.search) {
      const searchTerm = filters.search.toLowerCase();
      if (!user.userId.toLowerCase().includes(searchTerm)) {
        return false;
      }
    }
    
    // Start User ID filter (for pagination)
    if (filters.startUserId) {
      if (user.userId < filters.startUserId) {
        return false;
      }
    }
    
    return true;
  });
};

export const sortUsers = (users: User[], sortField: UserSortField, direction: 'asc' | 'desc' = 'asc'): User[] => {
  return [...users].sort((a, b) => {
    let aValue: string | number;
    let bValue: string | number;
    
    switch (sortField) {
      case 'userId':
        aValue = a.userId.toLowerCase();
        bValue = b.userId.toLowerCase();
        break;
      case 'firstName':
        aValue = a.firstName.toLowerCase();
        bValue = b.firstName.toLowerCase();
        break;
      case 'lastName':
        aValue = a.lastName.toLowerCase();
        bValue = b.lastName.toLowerCase();
        break;
      case 'userType':
        aValue = a.userType;
        bValue = b.userType;
        break;
      case 'createdAt':
      case 'updatedAt':
        aValue = new Date(a[sortField]).getTime();
        bValue = new Date(b[sortField]).getTime();
        break;
      default:
        return 0;
    }
    
    if (aValue < bValue) {
      return direction === 'asc' ? -1 : 1;
    }
    if (aValue > bValue) {
      return direction === 'asc' ? 1 : -1;
    }
    return 0;
  });
};

// User data transformation utilities
export const transformUserToListItem = (user: User): UserListItem => {
  return {
    userId: user.userId,
    firstName: user.firstName,
    lastName: user.lastName,
    userType: user.userType,
    fullName: user.fullName
  };
};

export const transformUserToFormData = (user: User): UserFormData => {
  return {
    userId: user.userId,
    firstName: user.firstName,
    lastName: user.lastName,
    password: '', // Don't populate password
    userType: user.userType
  };
};

export const transformFormDataToCreateRequest = (data: UserFormData): CreateUserData => {
  return {
    userId: data.userId.trim().toUpperCase(),
    firstName: data.firstName.trim(),
    lastName: data.lastName.trim(),
    password: data.password.trim(),
    userType: data.userType.trim().toUpperCase()
  };
};

export const transformFormDataToUpdateRequest = (data: UserFormData): UpdateUserData => {
  return {
    firstName: data.firstName.trim(),
    lastName: data.lastName.trim(),
    password: data.password.trim(),
    userType: data.userType.trim().toUpperCase()
  };
};

// Pagination utilities
export const calculatePaginationInfo = (page: number, size: number, totalElements: number) => {
  const totalPages = Math.ceil(totalElements / size);
  const hasNext = !((page + 1) >= totalPages);
  const hasPrevious = page > 0;
  const startIndex = page * size;
  const endIndex = Math.min(startIndex + size, totalElements);
  
  return {
    totalPages,
    hasNext,
    hasPrevious,
    startIndex,
    endIndex,
    currentPage: page,
    pageSize: size,
    totalElements,
    first: page === 0,
    last: (page + 1) >= totalPages
  };
};

// Search and query utilities
export const buildSearchParams = (params: Pageable): URLSearchParams => {
  const searchParams = new URLSearchParams();
  
  // Pagination parameters
  if (params.page !== undefined) searchParams.set('page', params.page.toString());
  if (params.size !== undefined) searchParams.set('size', params.size.toString());
  if (params.sort && params.sort.length > 0) {
    params.sort.forEach(sortParam => searchParams.append('sort', sortParam));
  }
  
  return searchParams;
};

// User action utilities
export const isValidUserAction = (action: string): boolean => {
  return ['U', 'u', 'D', 'd'].includes(action);
};

export const normalizeUserAction = (action: string): 'U' | 'D' | null => {
  const upperAction = action.toUpperCase();
  if (upperAction === 'U') return 'U';
  if (upperAction === 'D') return 'D';
  return null;
};

export const getUserActionLabel = (action: string): string => {
  switch (action.toUpperCase()) {
    case 'U':
      return 'Update';
    case 'D':
      return 'Delete';
    default:
      return 'Unknown';
  }
};

// Field length validation
export const validateFieldLength = (value: string, maxLength: number, fieldName: string): string | null => {
  if (value.length > maxLength) {
    return `${fieldName} cannot exceed ${maxLength} characters`;
  }
  return null;
};

// Utility constants
export const DEFAULT_PAGE_SIZE = 10;
export const MAX_PAGE_SIZE = 100;
export const DEFAULT_SORT_FIELD: UserSortField = 'userId';
export const DEFAULT_SORT_DIRECTION = 'asc';

export const USER_TYPE_OPTIONS = [
  { value: 'A', label: 'A - Admin' },
  { value: 'R', label: 'R - Regular' }
];

export const MAX_FIELD_LENGTHS = {
  userId: 8,
  firstName: 20,
  lastName: 20,
  password: 8,
  userType: 1
};

// Debounce utility for search
export const debounce = <T extends (...args: any[]) => any>(
  func: T,
  wait: number
): ((...args: Parameters<T>) => void) => {
  let timeout: NodeJS.Timeout;
  
  return (...args: Parameters<T>) => {
    clearTimeout(timeout);
    timeout = setTimeout(() => func(...args), wait);
  };
};

// Local storage utilities for user preferences
export const saveUserPreferences = (preferences: Record<string, any>): void => {
  try {
    localStorage.setItem('userManagementPreferences', JSON.stringify(preferences));
  } catch (error) {
    console.warn('Failed to save user preferences:', error);
  }
};

export const loadUserPreferences = (): Record<string, any> => {
  try {
    const stored = localStorage.getItem('userManagementPreferences');
    return stored ? JSON.parse(stored) : {};
  } catch (error) {
    console.warn('Failed to load user preferences:', error);
    return {};
  }
};

// Export all utilities as default object
const userUtils = {
  // Validation
  validateUserId,
  validateName,
  validatePassword,
  validateUserType,
  validateUserForm,
  validateSignOnForm,
  validateFieldLength,
  
  // Formatting
  formatUserFullName,
  formatUserDisplayName,
  formatUserType,
  getUserTypeLabel,
  formatDate,
  formatDateTime,
  
  // Error handling
  parseApiError,
  formatApiError,
  getErrorMessage,
  
  // Filtering and sorting
  filterUsers,
  sortUsers,
  
  // Data transformation
  transformUserToListItem,
  transformUserToFormData,
  transformFormDataToCreateRequest,
  transformFormDataToUpdateRequest,
  
  // Pagination
  calculatePaginationInfo,
  
  // Search and query
  buildSearchParams,
  
  // User actions
  isValidUserAction,
  normalizeUserAction,
  getUserActionLabel,
  
  // Utilities
  debounce,
  saveUserPreferences,
  loadUserPreferences,
  
  // Constants
  DEFAULT_PAGE_SIZE,
  MAX_PAGE_SIZE,
  DEFAULT_SORT_FIELD,
  DEFAULT_SORT_DIRECTION,
  USER_TYPE_OPTIONS,
  MAX_FIELD_LENGTHS
};

export default userUtils;