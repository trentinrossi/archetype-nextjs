// src/utils/userValidation.ts

import {
  UserFormData,
  UserFormErrors,
  CreateUserRequest,
  UpdateUserRequest,
  UserLoginRequest,
  ChangePasswordRequest,
  LoginFormData,
  LoginFormErrors,
  ChangePasswordFormData,
  ChangePasswordFormErrors,
  UserType,
  USER_VALIDATION_RULES,
} from '@/types/user';

/**
 * COBOL-style field validation utilities for User Management System
 * Implements strict field validation rules following COBOL business logic
 */

// Field validation functions
export const validateUserId = (userId: string): string | undefined => {
  if (!userId || userId.trim().length === 0) {
    return 'User ID is required';
  }

  const trimmedUserId = userId.trim().toUpperCase();

  if (trimmedUserId.length > USER_VALIDATION_RULES.userId.maxLength) {
    return `User ID must be ${USER_VALIDATION_RULES.userId.maxLength} characters or less`;
  }

  if (!USER_VALIDATION_RULES.userId.pattern.test(trimmedUserId)) {
    return 'User ID must contain only alphanumeric characters (A-Z, 0-9)';
  }

  return undefined;
};

export const validateFirstName = (firstName: string): string | undefined => {
  if (!firstName || firstName.trim().length === 0) {
    return 'First Name is required';
  }

  const trimmedFirstName = firstName.trim();

  if (trimmedFirstName.length > USER_VALIDATION_RULES.firstName.maxLength) {
    return `First Name must be ${USER_VALIDATION_RULES.firstName.maxLength} characters or less`;
  }

  // COBOL-style character validation - only allow letters, spaces, hyphens, apostrophes
  const namePattern = /^[A-Za-z\s\-'\.]+$/;
  if (!namePattern.test(trimmedFirstName)) {
    return 'First Name must contain only letters, spaces, hyphens, and apostrophes';
  }

  return undefined;
};

export const validateLastName = (lastName: string): string | undefined => {
  if (!lastName || lastName.trim().length === 0) {
    return 'Last Name is required';
  }

  const trimmedLastName = lastName.trim();

  if (trimmedLastName.length > USER_VALIDATION_RULES.lastName.maxLength) {
    return `Last Name must be ${USER_VALIDATION_RULES.lastName.maxLength} characters or less`;
  }

  // COBOL-style character validation - only allow letters, spaces, hyphens, apostrophes
  const namePattern = /^[A-Za-z\s\-'\.]+$/;
  if (!namePattern.test(trimmedLastName)) {
    return 'Last Name must contain only letters, spaces, hyphens, and apostrophes';
  }

  return undefined;
};

export const validatePassword = (password: string): string | undefined => {
  if (!password) {
    return 'Password is required';
  }

  if (password.length !== USER_VALIDATION_RULES.password.minLength) {
    return `Password must be exactly ${USER_VALIDATION_RULES.password.minLength} characters`;
  }

  if (!USER_VALIDATION_RULES.password.pattern.test(password)) {
    return 'Password must contain at least one uppercase letter, one lowercase letter, one number, and one special character (@$!%*?&)';
  }

  // Additional COBOL-style password rules
  if (password.includes(' ')) {
    return 'Password cannot contain spaces';
  }

  // Check for sequential characters (COBOL business rule)
  const hasSequential = /(?:abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz|123|234|345|456|567|678|789)/i.test(password);
  if (hasSequential) {
    return 'Password cannot contain sequential characters';
  }

  // Check for repeated characters (COBOL business rule)
  const hasRepeated = /(.)\1{2,}/.test(password);
  if (hasRepeated) {
    return 'Password cannot contain more than 2 consecutive identical characters';
  }

  return undefined;
};

export const validateConfirmPassword = (password: string, confirmPassword: string): string | undefined => {
  if (!confirmPassword) {
    return 'Confirm Password is required';
  }

  if (confirmPassword !== password) {
    return 'Passwords do not match';
  }

  return undefined;
};

export const validateUserType = (userType: string): string | undefined => {
  if (!userType) {
    return 'User Type is required';
  }

  if (!USER_VALIDATION_RULES.userType.allowedValues.includes(userType as UserType)) {
    return 'Invalid User Type selected';
  }

  return undefined;
};

// Form validation functions
export const validateUserForm = (formData: UserFormData): UserFormErrors => {
  const errors: UserFormErrors = {};

  const userIdError = validateUserId(formData.userId);
  if (userIdError) errors.userId = userIdError;

  const firstNameError = validateFirstName(formData.firstName);
  if (firstNameError) errors.firstName = firstNameError;

  const lastNameError = validateLastName(formData.lastName);
  if (lastNameError) errors.lastName = lastNameError;

  const passwordError = validatePassword(formData.password);
  if (passwordError) errors.password = passwordError;

  const confirmPasswordError = validateConfirmPassword(formData.password, formData.confirmPassword);
  if (confirmPasswordError) errors.confirmPassword = confirmPasswordError;

  const userTypeError = validateUserType(formData.userType);
  if (userTypeError) errors.userType = userTypeError;

  return errors;
};

export const validateLoginForm = (formData: LoginFormData): LoginFormErrors => {
  const errors: LoginFormErrors = {};

  const userIdError = validateUserId(formData.userId);
  if (userIdError) errors.userId = userIdError;

  if (!formData.password) {
    errors.password = 'Password is required';
  } else if (formData.password.length !== 8) {
    errors.password = 'Password must be exactly 8 characters';
  }

  return errors;
};

export const validateChangePasswordForm = (formData: ChangePasswordFormData): ChangePasswordFormErrors => {
  const errors: ChangePasswordFormErrors = {};

  if (!formData.currentPassword) {
    errors.currentPassword = 'Current Password is required';
  } else if (formData.currentPassword.length !== 8) {
    errors.currentPassword = 'Current Password must be exactly 8 characters';
  }

  const newPasswordError = validatePassword(formData.newPassword);
  if (newPasswordError) errors.newPassword = newPasswordError;

  const confirmPasswordError = validateConfirmPassword(formData.newPassword, formData.confirmNewPassword);
  if (confirmPasswordError) errors.confirmNewPassword = confirmPasswordError;

  // COBOL business rule: new password cannot be same as current password
  if (formData.currentPassword && formData.newPassword && formData.currentPassword === formData.newPassword) {
    errors.newPassword = 'New password must be different from current password';
  }

  return errors;
};

// Request validation functions
export const validateCreateUserRequest = (request: CreateUserRequest): string[] => {
  const errors: string[] = [];

  const userIdError = validateUserId(request.userId);
  if (userIdError) errors.push(`User ID: ${userIdError}`);

  const firstNameError = validateFirstName(request.firstName);
  if (firstNameError) errors.push(`First Name: ${firstNameError}`);

  const lastNameError = validateLastName(request.lastName);
  if (lastNameError) errors.push(`Last Name: ${lastNameError}`);

  const passwordError = validatePassword(request.password);
  if (passwordError) errors.push(`Password: ${passwordError}`);

  const userTypeError = validateUserType(request.userType);
  if (userTypeError) errors.push(`User Type: ${userTypeError}`);

  return errors;
};

export const validateUpdateUserRequest = (request: UpdateUserRequest): string[] => {
  const errors: string[] = [];

  if (request.firstName !== undefined) {
    const firstNameError = validateFirstName(request.firstName);
    if (firstNameError) errors.push(`First Name: ${firstNameError}`);
  }

  if (request.lastName !== undefined) {
    const lastNameError = validateLastName(request.lastName);
    if (lastNameError) errors.push(`Last Name: ${lastNameError}`);
  }

  if (request.password !== undefined) {
    const passwordError = validatePassword(request.password);
    if (passwordError) errors.push(`Password: ${passwordError}`);
  }

  if (request.userType !== undefined) {
    const userTypeError = validateUserType(request.userType);
    if (userTypeError) errors.push(`User Type: ${userTypeError}`);
  }

  return errors;
};

export const validateLoginRequest = (request: UserLoginRequest): string[] => {
  const errors: string[] = [];

  const userIdError = validateUserId(request.userId);
  if (userIdError) errors.push(`User ID: ${userIdError}`);

  if (!request.password) {
    errors.push('Password: Password is required');
  } else if (request.password.length !== 8) {
    errors.push('Password: Password must be exactly 8 characters');
  }

  return errors;
};

export const validateChangePasswordRequest = (request: ChangePasswordRequest): string[] => {
  const errors: string[] = [];

  const userIdError = validateUserId(request.userId);
  if (userIdError) errors.push(`User ID: ${userIdError}`);

  if (!request.currentPassword) {
    errors.push('Current Password: Current password is required');
  } else if (request.currentPassword.length !== 8) {
    errors.push('Current Password: Current password must be exactly 8 characters');
  }

  const newPasswordError = validatePassword(request.newPassword);
  if (newPasswordError) errors.push(`New Password: ${newPasswordError}`);

  // COBOL business rule: new password cannot be same as current password
  if (request.currentPassword && request.newPassword && request.currentPassword === request.newPassword) {
    errors.push('New Password: New password must be different from current password');
  }

  return errors;
};

// Utility functions for form handling
export const isFormValid = (errors: UserFormErrors | LoginFormErrors | ChangePasswordFormErrors): boolean => {
  return Object.keys(errors).length === 0 || Object.values(errors).every(error => !error);
};

export const getFirstError = (errors: UserFormErrors | LoginFormErrors | ChangePasswordFormErrors): string | undefined => {
  const errorValues = Object.values(errors).filter(error => error);
  return errorValues.length > 0 ? errorValues[0] : undefined;
};

export const hasFieldError = (errors: UserFormErrors | LoginFormErrors | ChangePasswordFormErrors, field: string): boolean => {
  return !!(errors as any)[field];
};

export const getFieldError = (errors: UserFormErrors | LoginFormErrors | ChangePasswordFormErrors, field: string): string | undefined => {
  return (errors as any)[field];
};

// Data sanitization functions (COBOL-style)
export const sanitizeUserId = (userId: string): string => {
  return userId.trim().toUpperCase().replace(/[^A-Z0-9]/g, '').substring(0, 8);
};

export const sanitizeName = (name: string): string => {
  return name.trim().replace(/[^A-Za-z\s\-'\.]/g, '').substring(0, 20);
};

export const sanitizeUserType = (userType: string): UserType | undefined => {
  const upperType = userType.toUpperCase();
  return Object.values(UserType).find(type => type === upperType);
};

// Form data preparation functions
export const prepareCreateUserRequest = (formData: UserFormData): CreateUserRequest => {
  return {
    userId: sanitizeUserId(formData.userId),
    firstName: sanitizeName(formData.firstName),
    lastName: sanitizeName(formData.lastName),
    password: formData.password,
    userType: formData.userType,
  };
};

export const prepareUpdateUserRequest = (formData: Partial<UserFormData>): UpdateUserRequest => {
  const request: UpdateUserRequest = {};

  if (formData.firstName !== undefined) {
    request.firstName = sanitizeName(formData.firstName);
  }

  if (formData.lastName !== undefined) {
    request.lastName = sanitizeName(formData.lastName);
  }

  if (formData.password !== undefined) {
    request.password = formData.password;
  }

  if (formData.userType !== undefined) {
    request.userType = formData.userType;
  }

  return request;
};

export const prepareLoginRequest = (formData: LoginFormData): UserLoginRequest => {
  return {
    userId: sanitizeUserId(formData.userId),
    password: formData.password,
  };
};

export const prepareChangePasswordRequest = (userId: string, formData: ChangePasswordFormData): ChangePasswordRequest => {
  return {
    userId: sanitizeUserId(userId),
    currentPassword: formData.currentPassword,
    newPassword: formData.newPassword,
  };
};

// Password strength checker (COBOL business rules)
export const checkPasswordStrength = (password: string): {
  score: number;
  feedback: string[];
  isStrong: boolean;
} => {
  const feedback: string[] = [];
  let score = 0;

  if (password.length === 8) {
    score += 20;
  } else {
    feedback.push('Password must be exactly 8 characters');
  }

  if (/[A-Z]/.test(password)) {
    score += 20;
  } else {
    feedback.push('Add at least one uppercase letter');
  }

  if (/[a-z]/.test(password)) {
    score += 20;
  } else {
    feedback.push('Add at least one lowercase letter');
  }

  if (/\d/.test(password)) {
    score += 20;
  } else {
    feedback.push('Add at least one number');
  }

  if (/[@$!%*?&]/.test(password)) {
    score += 20;
  } else {
    feedback.push('Add at least one special character (@$!%*?&)');
  }

  // Deduct points for common patterns
  if (/(.)\1{2,}/.test(password)) {
    score -= 10;
    feedback.push('Avoid repeating characters');
  }

  if (/(?:abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz|123|234|345|456|567|678|789)/i.test(password)) {
    score -= 10;
    feedback.push('Avoid sequential characters');
  }

  if (password.includes(' ')) {
    score -= 20;
    feedback.push('Remove spaces from password');
  }

  const isStrong = score >= 80 && feedback.length === 0;

  return {
    score: Math.max(0, Math.min(100, score)),
    feedback,
    isStrong,
  };
};

// Field formatting functions (COBOL-style)
export const formatUserId = (userId: string): string => {
  return userId.toUpperCase().padEnd(8, ' ').substring(0, 8);
};

export const formatName = (name: string): string => {
  return name.trim().padEnd(20, ' ').substring(0, 20);
};

export const formatUserTypeForDisplay = (userType: UserType): string => {
  const labels: Record<UserType, string> = {
    [UserType.ADMIN]: 'Administrator',
    [UserType.USER]: 'User',
    [UserType.GUEST]: 'Guest',
    [UserType.MANAGER]: 'Manager',
    [UserType.OPERATOR]: 'Operator',
  };
  return labels[userType] || userType;
};

// Validation summary functions
export const getValidationSummary = (errors: string[]): string => {
  if (errors.length === 0) {
    return 'All fields are valid';
  }

  if (errors.length === 1) {
    return `1 validation error: ${errors[0]}`;
  }

  return `${errors.length} validation errors found`;
};

export const formatValidationErrors = (errors: string[]): string => {
  if (errors.length === 0) {
    return '';
  }

  return errors.map((error, index) => `${index + 1}. ${error}`).join('\n');
};

// COBOL-style field comparison functions
export const compareUserIds = (userId1: string, userId2: string): number => {
  const formatted1 = formatUserId(userId1);
  const formatted2 = formatUserId(userId2);
  return formatted1.localeCompare(formatted2);
};

export const compareNames = (name1: string, name2: string): number => {
  const formatted1 = formatName(name1);
  const formatted2 = formatName(name2);
  return formatted1.localeCompare(formatted2);
};

// Export all validation utilities
export const userValidationUtils = {
  // Field validators
  validateUserId,
  validateFirstName,
  validateLastName,
  validatePassword,
  validateConfirmPassword,
  validateUserType,

  // Form validators
  validateUserForm,
  validateLoginForm,
  validateChangePasswordForm,

  // Request validators
  validateCreateUserRequest,
  validateUpdateUserRequest,
  validateLoginRequest,
  validateChangePasswordRequest,

  // Utility functions
  isFormValid,
  getFirstError,
  hasFieldError,
  getFieldError,

  // Data sanitization
  sanitizeUserId,
  sanitizeName,
  sanitizeUserType,

  // Request preparation
  prepareCreateUserRequest,
  prepareUpdateUserRequest,
  prepareLoginRequest,
  prepareChangePasswordRequest,

  // Password utilities
  checkPasswordStrength,

  // Formatting
  formatUserId,
  formatName,
  formatUserTypeForDisplay,

  // Validation summary
  getValidationSummary,
  formatValidationErrors,

  // Comparison
  compareUserIds,
  compareNames,
};

export default userValidationUtils;