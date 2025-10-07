// src/types/user-security.ts

export interface SignonRequestDTO {
  userId: string;
  password: string;
  systemId?: string;
  workstationId?: string;
  transactionId?: string;
}

export interface UserSecurityDTO {
  userId: string;
  userName: string;
  userType: 'ADMIN' | 'USER' | 'GUEST' | 'SYSTEM';
  userStatus: 'ACTIVE' | 'INACTIVE' | 'LOCKED' | 'SUSPENDED';
  passwordExpired: boolean;
  passwordExpiryDate?: string;
  lastSignonDate?: string;
  lastSignonTime?: string;
  failedSignonAttempts: number;
  maxFailedAttempts: number;
  accountLocked: boolean;
  lockoutDate?: string;
  lockoutTime?: string;
  securityLevel: number;
  userGroup?: string;
  department?: string;
  email?: string;
  phoneNumber?: string;
  createdDate: string;
  createdTime: string;
  createdBy: string;
  lastModifiedDate?: string;
  lastModifiedTime?: string;
  lastModifiedBy?: string;
  profileData?: UserProfileData;
  permissions: UserPermission[];
  roles: UserRole[];
}

export interface UserProfileData {
  firstName?: string;
  lastName?: string;
  middleName?: string;
  displayName?: string;
  title?: string;
  employeeId?: string;
  costCenter?: string;
  location?: string;
  timezone?: string;
  language?: string;
  dateFormat?: string;
  timeFormat?: string;
}

export interface UserPermission {
  permissionId: string;
  permissionName: string;
  resourceType: string;
  resourceId?: string;
  accessLevel: 'READ' | 'WRITE' | 'DELETE' | 'ADMIN';
  granted: boolean;
  grantedDate: string;
  grantedBy: string;
  expiryDate?: string;
}

export interface UserRole {
  roleId: string;
  roleName: string;
  roleDescription?: string;
  roleType: 'FUNCTIONAL' | 'ADMINISTRATIVE' | 'SYSTEM';
  assigned: boolean;
  assignedDate: string;
  assignedBy: string;
  expiryDate?: string;
}

export interface CreateUserSecurityRequest {
  userId: string;
  userName: string;
  password: string;
  confirmPassword: string;
  userType: 'ADMIN' | 'USER' | 'GUEST' | 'SYSTEM';
  userStatus?: 'ACTIVE' | 'INACTIVE';
  securityLevel: number;
  userGroup?: string;
  department?: string;
  email?: string;
  phoneNumber?: string;
  profileData?: Partial<UserProfileData>;
  roleIds?: string[];
  permissionIds?: string[];
  passwordExpiryDays?: number;
  maxFailedAttempts?: number;
}

export interface UpdateUserSecurityRequest {
  userName?: string;
  userType?: 'ADMIN' | 'USER' | 'GUEST' | 'SYSTEM';
  userStatus?: 'ACTIVE' | 'INACTIVE' | 'LOCKED' | 'SUSPENDED';
  securityLevel?: number;
  userGroup?: string;
  department?: string;
  email?: string;
  phoneNumber?: string;
  profileData?: Partial<UserProfileData>;
  roleIds?: string[];
  permissionIds?: string[];
  maxFailedAttempts?: number;
  resetFailedAttempts?: boolean;
  unlockAccount?: boolean;
  extendPasswordExpiry?: boolean;
  passwordExpiryDays?: number;
}

export interface ChangePasswordRequest {
  userId: string;
  currentPassword: string;
  newPassword: string;
  confirmNewPassword: string;
  forcePasswordChange?: boolean;
  passwordExpiryDays?: number;
}

export interface ValidationResponseDTO {
  isValid: boolean;
  validationCode: string;
  validationMessage: string;
  validationDetails?: ValidationDetail[];
  timestamp: string;
  sessionId?: string;
  expiryTime?: string;
}

export interface ValidationDetail {
  field: string;
  code: string;
  message: string;
  severity: 'ERROR' | 'WARNING' | 'INFO';
}

export interface SignonResponseDTO {
  success: boolean;
  userId: string;
  userName: string;
  sessionId: string;
  sessionToken: string;
  tokenType: 'Bearer';
  expiresIn: number;
  expiryTime: string;
  userSecurity: UserSecurityDTO;
  passwordChangeRequired: boolean;
  firstTimeLogin: boolean;
  lastSignonDate?: string;
  lastSignonTime?: string;
  message?: string;
  warnings?: string[];
}

export interface ExitRequestDTO {
  userId: string;
  sessionId: string;
  sessionToken: string;
}

export interface ExitResponseDTO {
  success: boolean;
  message: string;
  timestamp: string;
}

export interface InvalidKeyRequestDTO {
  userId: string;
  invalidKey: string;
  reason?: string;
}

export interface InvalidKeyResponseDTO {
  acknowledged: boolean;
  message: string;
  timestamp: string;
  actionTaken?: string;
}

export interface UserSecurityListResponse {
  users: UserSecurityDTO[];
  totalCount: number;
  pageSize: number;
  currentPage: number;
  totalPages: number;
  hasNextPage: boolean;
  hasPreviousPage: boolean;
}

export interface UserSecuritySearchCriteria {
  userId?: string;
  userName?: string;
  userType?: 'ADMIN' | 'USER' | 'GUEST' | 'SYSTEM';
  userStatus?: 'ACTIVE' | 'INACTIVE' | 'LOCKED' | 'SUSPENDED';
  userGroup?: string;
  department?: string;
  securityLevel?: number;
  accountLocked?: boolean;
  passwordExpired?: boolean;
  createdDateFrom?: string;
  createdDateTo?: string;
  lastSignonDateFrom?: string;
  lastSignonDateTo?: string;
  page?: number;
  pageSize?: number;
  sortBy?: string;
  sortOrder?: 'ASC' | 'DESC';
}

export interface PasswordPolicy {
  minLength: number;
  maxLength: number;
  requireUppercase: boolean;
  requireLowercase: boolean;
  requireNumbers: boolean;
  requireSpecialChars: boolean;
  specialCharsAllowed: string;
  preventReuse: boolean;
  reuseHistoryCount: number;
  expiryDays: number;
  warningDays: number;
  maxFailedAttempts: number;
  lockoutDurationMinutes: number;
}

export interface SecurityAuditLog {
  logId: string;
  userId: string;
  action: 'SIGNON' | 'SIGNOFF' | 'PASSWORD_CHANGE' | 'ACCOUNT_LOCK' | 'ACCOUNT_UNLOCK' | 'PERMISSION_CHANGE' | 'ROLE_CHANGE';
  actionDetails: string;
  result: 'SUCCESS' | 'FAILURE' | 'WARNING';
  ipAddress?: string;
  userAgent?: string;
  sessionId?: string;
  timestamp: string;
  performedBy?: string;
}

export interface APIError {
  error: string;
  message: string;
  statusCode: number;
  timestamp: string;
  path: string;
  details?: ValidationDetail[];
}

export interface HTTPValidationError {
  detail: Array<{
    loc: Array<string | number>;
    msg: string;
    type: string;
  }>;
}

export type UserSecurityStatus = 'ACTIVE' | 'INACTIVE' | 'LOCKED' | 'SUSPENDED';
export type UserSecurityType = 'ADMIN' | 'USER' | 'GUEST' | 'SYSTEM';
export type PermissionAccessLevel = 'READ' | 'WRITE' | 'DELETE' | 'ADMIN';
export type RoleType = 'FUNCTIONAL' | 'ADMINISTRATIVE' | 'SYSTEM';
export type ValidationSeverity = 'ERROR' | 'WARNING' | 'INFO';
export type AuditAction = 'SIGNON' | 'SIGNOFF' | 'PASSWORD_CHANGE' | 'ACCOUNT_LOCK' | 'ACCOUNT_UNLOCK' | 'PERMISSION_CHANGE' | 'ROLE_CHANGE';
export type AuditResult = 'SUCCESS' | 'FAILURE' | 'WARNING';
export type SortOrder = 'ASC' | 'DESC';