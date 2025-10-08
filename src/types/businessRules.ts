// Business Rules Types for User Management System
// Defines TypeScript interfaces and types for COUSR00C, COUSR01C, COUSR02C, COUSR03C, and COADM01C

// Program Navigation Types
export interface ProgramNavigation {
  currentProgram: string;
  previousProgram?: string;
  nextProgram?: string;
  menuLevel: number;
  breadcrumb: string[];
}

export interface MenuOption {
  id: string;
  label: string;
  description: string;
  programId: string;
  functionKey?: string;
  isEnabled: boolean;
  requiresPermission?: string[];
}

// COADM01C - Admin Menu Types
export interface COADM01CState {
  selectedOption: string;
  availableOptions: MenuOption[];
  currentUser: string;
  currentDateTime: string;
  lastActivity: string;
  sessionTimeout: number;
  isLoading: boolean;
  error: string;
  message: string;
}

export interface COADM01CRequest {
  action: 'DISPLAY_MENU' | 'SELECT_OPTION' | 'EXIT';
  selectedOption?: string;
  userId: string;
  sessionId: string;
}

export interface COADM01CResponse {
  returnCode: string;
  message: string;
  menuOptions: MenuOption[];
  selectedProgram?: string;
  navigationPath?: string;
}

// COUSR00C - User List Types
export interface COUSR00CState {
  isLoading: boolean;
  isSearching: boolean;
  error: string;
  message: string;
  currentDateTime: string;
  users: UserListItem[];
  currentPage: number;
  totalPages: number;
  totalUsers: number;
  pageSize: number;
  hasNext: boolean;
  hasPrev: boolean;
  searchUserId: string;
  selectedActions: { [key: string]: UserAction };
  sortBy: string;
  sortOrder: 'asc' | 'desc';
  filterCriteria: UserFilterCriteria;
}

export interface UserListItem {
  userId: string;
  firstName: string;
  lastName: string;
  email: string;
  userType: string;
  status: UserStatus;
  lastLoginDate?: string;
  createdDate: string;
  isSelected: boolean;
  action: UserAction;
}

export interface UserFilterCriteria {
  startingUserId?: string;
  userType?: string;
  status?: UserStatus;
  createdDateFrom?: string;
  createdDateTo?: string;
  lastLoginFrom?: string;
  lastLoginTo?: string;
}

export type UserAction = '' | 'U' | 'D' | 'V' | 'A' | 'I';

export interface COUSR00CRequest {
  action: 'LIST' | 'SEARCH' | 'SORT' | 'FILTER' | 'PAGE_NEXT' | 'PAGE_PREV';
  searchCriteria?: UserFilterCriteria;
  pagination: {
    page: number;
    limit: number;
  };
  sorting?: {
    field: string;
    order: 'asc' | 'desc';
  };
}

export interface COUSR00CResponse {
  returnCode: string;
  message: string;
  users: UserListItem[];
  pagination: PaginationInfo;
  totalCount: number;
  hasMoreData: boolean;
}

// COUSR01C - Add User Types
export interface COUSR01CState {
  isLoading: boolean;
  isSaving: boolean;
  error: string;
  message: string;
  currentDateTime: string;
  formData: CreateUserFormData;
  formErrors: FormValidationErrors;
  availableRoles: RoleOption[];
  availableUserTypes: UserTypeOption[];
  passwordPolicy: PasswordPolicy;
  isDirty: boolean;
  isValid: boolean;
}

export interface CreateUserFormData {
  userId: string;
  firstName: string;
  lastName: string;
  email: string;
  userType: string;
  roles: string[];
  password: string;
  confirmPassword: string;
  isActive: boolean;
  mustChangePassword: boolean;
  passwordExpiresAt?: string;
  department?: string;
  phoneNumber?: string;
  notes?: string;
}

export interface COUSR01CRequest {
  action: 'CREATE' | 'VALIDATE' | 'CANCEL' | 'RESET';
  userData?: CreateUserFormData;
  validationOnly?: boolean;
}

export interface COUSR01CResponse {
  returnCode: string;
  message: string;
  createdUser?: UserSecurity;
  validationErrors?: FormValidationErrors;
  generatedUserId?: string;
}

// COUSR02C - Update User Types
export interface COUSR02CState {
  isLoading: boolean;
  isSaving: boolean;
  isLoadingUser: boolean;
  error: string;
  message: string;
  currentDateTime: string;
  originalUserData: UserSecurity | null;
  formData: UpdateUserFormData;
  formErrors: FormValidationErrors;
  availableRoles: RoleOption[];
  availableUserTypes: UserTypeOption[];
  isDirty: boolean;
  isValid: boolean;
  hasUnsavedChanges: boolean;
}

export interface UpdateUserFormData {
  userId: string;
  firstName: string;
  lastName: string;
  email: string;
  userType: string;
  roles: string[];
  isActive: boolean;
  mustChangePassword: boolean;
  passwordExpiresAt?: string;
  department?: string;
  phoneNumber?: string;
  notes?: string;
  lastModifiedBy?: string;
  lastModifiedAt?: string;
}

export interface COUSR02CRequest {
  action: 'LOAD' | 'UPDATE' | 'VALIDATE' | 'CANCEL' | 'RESET';
  userId: string;
  userData?: UpdateUserFormData;
  validationOnly?: boolean;
}

export interface COUSR02CResponse {
  returnCode: string;
  message: string;
  user?: UserSecurity;
  updatedUser?: UserSecurity;
  validationErrors?: FormValidationErrors;
  changesSummary?: ChangesSummary;
}

// COUSR03C - Delete User Types
export interface COUSR03CState {
  isLoading: boolean;
  isDeleting: boolean;
  isLoadingUser: boolean;
  error: string;
  message: string;
  currentDateTime: string;
  userData: UserSecurity | null;
  confirmationRequired: boolean;
  confirmationText: string;
  deleteReason: string;
  deleteOptions: DeleteOption[];
  selectedDeleteOption: string;
  relatedData: RelatedUserData;
  canDelete: boolean;
  deleteWarnings: string[];
}

export interface DeleteOption {
  id: string;
  label: string;
  description: string;
  requiresReason: boolean;
  isDestructive: boolean;
}

export interface RelatedUserData {
  activeSessionCount: number;
  lastLoginDate?: string;
  createdRecordsCount: number;
  assignedRolesCount: number;
  dependentUsersCount: number;
  hasActiveTransactions: boolean;
}

export interface COUSR03CRequest {
  action: 'LOAD' | 'DELETE' | 'SOFT_DELETE' | 'DEACTIVATE' | 'CANCEL';
  userId: string;
  deleteReason?: string;
  deleteOption?: string;
  forceDelete?: boolean;
}

export interface COUSR03CResponse {
  returnCode: string;
  message: string;
  user?: UserSecurity;
  deletedUser?: UserSecurity;
  relatedData?: RelatedUserData;
  warnings?: string[];
  canDelete?: boolean;
}

// Common Form Types
export interface FormValidationErrors {
  [fieldName: string]: string[];
}

export interface FormFieldState {
  value: any;
  error: string[];
  touched: boolean;
  dirty: boolean;
  valid: boolean;
}

export interface FormState {
  fields: { [fieldName: string]: FormFieldState };
  isValid: boolean;
  isDirty: boolean;
  isSubmitting: boolean;
  submitCount: number;
}

// Pagination Types
export interface PaginationInfo {
  currentPage: number;
  totalPages: number;
  pageSize: number;
  totalItems: number;
  hasNext: boolean;
  hasPrev: boolean;
  startIndex: number;
  endIndex: number;
}

export interface PaginationRequest {
  page: number;
  limit: number;
  offset?: number;
}

export interface PaginationControls {
  goToPage: (page: number) => void;
  goToNext: () => void;
  goToPrev: () => void;
  goToFirst: () => void;
  goToLast: () => void;
  changePageSize: (size: number) => void;
}

// Business Rule Validation Types
export interface ValidationRule {
  field: string;
  type: 'required' | 'minLength' | 'maxLength' | 'pattern' | 'custom' | 'unique';
  value?: any;
  message: string;
  condition?: (formData: any) => boolean;
}

export interface ValidationResult {
  isValid: boolean;
  errors: FormValidationErrors;
  warnings?: string[];
}

export interface PasswordPolicy {
  minLength: number;
  maxLength: number;
  requireUppercase: boolean;
  requireLowercase: boolean;
  requireNumbers: boolean;
  requireSpecialChars: boolean;
  forbiddenPatterns: string[];
  expirationDays: number;
  historyCount: number;
}

// User Management Business Rules
export interface UserBusinessRules {
  userIdPattern: RegExp;
  userIdMinLength: number;
  userIdMaxLength: number;
  emailPattern: RegExp;
  nameMinLength: number;
  nameMaxLength: number;
  maxRolesPerUser: number;
  defaultUserType: string;
  allowDuplicateEmails: boolean;
  requireUniqueUserIds: boolean;
  passwordPolicy: PasswordPolicy;
}

// Role and Permission Types
export interface RoleOption {
  id: string;
  name: string;
  description: string;
  permissions: string[];
  isActive: boolean;
  isDefault: boolean;
  level: number;
}

export interface UserTypeOption {
  id: string;
  name: string;
  description: string;
  defaultRoles: string[];
  isActive: boolean;
  sortOrder: number;
}

export interface PermissionOption {
  id: string;
  name: string;
  description: string;
  category: string;
  isActive: boolean;
}

// Navigation and Program Flow Types
export interface ProgramFlow {
  currentStep: number;
  totalSteps: number;
  canGoNext: boolean;
  canGoPrev: boolean;
  canCancel: boolean;
  canSave: boolean;
  nextAction?: string;
  prevAction?: string;
}

export interface NavigationState {
  currentProgram: string;
  programStack: string[];
  returnProgram?: string;
  parameters: { [key: string]: any };
  breadcrumb: BreadcrumbItem[];
}

export interface BreadcrumbItem {
  label: string;
  program: string;
  parameters?: { [key: string]: any };
  isClickable: boolean;
}

// Function Key Mapping Types
export interface FunctionKeyMapping {
  key: string;
  label: string;
  action: string;
  isEnabled: boolean;
  requiresConfirmation?: boolean;
  confirmationMessage?: string;
}

export interface ProgramFunctionKeys {
  [programId: string]: FunctionKeyMapping[];
}

// Screen State Management Types
export interface ScreenState {
  programId: string;
  screenId: string;
  mode: 'VIEW' | 'ADD' | 'UPDATE' | 'DELETE' | 'SEARCH';
  isLoading: boolean;
  hasUnsavedChanges: boolean;
  lastSaved?: string;
  currentUser: string;
  sessionId: string;
}

export interface ScreenTransition {
  fromScreen: string;
  toScreen: string;
  action: string;
  parameters?: { [key: string]: any };
  saveRequired?: boolean;
}

// Audit and Change Tracking Types
export interface ChangesSummary {
  changedFields: ChangedField[];
  totalChanges: number;
  changeType: 'CREATE' | 'UPDATE' | 'DELETE';
  timestamp: string;
  userId: string;
}

export interface ChangedField {
  fieldName: string;
  fieldLabel: string;
  oldValue: any;
  newValue: any;
  changeType: 'ADDED' | 'MODIFIED' | 'REMOVED';
}

export interface AuditTrail {
  id: string;
  entityType: string;
  entityId: string;
  action: string;
  changes: ChangedField[];
  userId: string;
  timestamp: string;
  ipAddress?: string;
  userAgent?: string;
}

// Error Handling Types
export interface BusinessRuleError {
  code: string;
  message: string;
  field?: string;
  severity: 'ERROR' | 'WARNING' | 'INFO';
  details?: { [key: string]: any };
}

export interface ProgramError {
  programId: string;
  errorCode: string;
  errorMessage: string;
  errorDetails?: string;
  timestamp: string;
  userId?: string;
  stackTrace?: string;
}

// Session and Security Types
export interface UserSession {
  sessionId: string;
  userId: string;
  startTime: string;
  lastActivity: string;
  ipAddress: string;
  userAgent: string;
  isActive: boolean;
  timeoutMinutes: number;
  currentProgram?: string;
}

export interface SecurityContext {
  userId: string;
  roles: string[];
  permissions: string[];
  sessionId: string;
  isAuthenticated: boolean;
  canAccess: (resource: string, action: string) => boolean;
}

// API Integration Types
export interface BusinessRuleRequest<T = any> {
  programId: string;
  action: string;
  data?: T;
  userId: string;
  sessionId: string;
  timestamp: string;
}

export interface BusinessRuleResponse<T = any> {
  success: boolean;
  returnCode: string;
  message: string;
  data?: T;
  errors?: BusinessRuleError[];
  warnings?: string[];
  timestamp: string;
}

// Hook Types for Business Rules
export interface UseBusinessRulesReturn {
  validateForm: (formData: any, rules: ValidationRule[]) => ValidationResult;
  executeBusinessRule: (request: BusinessRuleRequest) => Promise<BusinessRuleResponse>;
  checkPermissions: (resource: string, action: string) => boolean;
  auditAction: (action: string, entityId: string, changes: ChangedField[]) => Promise<void>;
  isLoading: boolean;
  error: string | null;
}

export interface UseProgramNavigationReturn {
  currentProgram: string;
  navigationState: NavigationState;
  navigateToProgram: (programId: string, parameters?: any) => void;
  goBack: () => void;
  updateBreadcrumb: (item: BreadcrumbItem) => void;
  canNavigateAway: () => boolean;
}

export interface UsePaginationReturn {
  paginationInfo: PaginationInfo;
  paginationControls: PaginationControls;
  isLoading: boolean;
  error: string | null;
}

// Constants and Enums
export enum ProgramId {
  COADM01C = 'COADM01C',
  COUSR00C = 'COUSR00C',
  COUSR01C = 'COUSR01C',
  COUSR02C = 'COUSR02C',
  COUSR03C = 'COUSR03C',
  COSGN00C = 'COSGN00C'
}

export enum ActionType {
  DISPLAY = 'DISPLAY',
  CREATE = 'CREATE',
  UPDATE = 'UPDATE',
  DELETE = 'DELETE',
  SEARCH = 'SEARCH',
  VALIDATE = 'VALIDATE',
  CANCEL = 'CANCEL',
  EXIT = 'EXIT',
  SAVE = 'SAVE',
  RESET = 'RESET'
}

export enum ReturnCode {
  SUCCESS = '00',
  WARNING = '04',
  ERROR = '08',
  FATAL = '12',
  NOT_FOUND = '16',
  DUPLICATE = '20',
  VALIDATION_ERROR = '24',
  PERMISSION_DENIED = '28',
  SESSION_EXPIRED = '32'
}

export enum ScreenMode {
  VIEW = 'VIEW',
  ADD = 'ADD',
  UPDATE = 'UPDATE',
  DELETE = 'DELETE',
  SEARCH = 'SEARCH',
  BROWSE = 'BROWSE'
}

// Type Guards
export const isValidReturnCode = (code: string): code is ReturnCode => {
  return Object.values(ReturnCode).includes(code as ReturnCode);
};

export const isValidProgramId = (id: string): id is ProgramId => {
  return Object.values(ProgramId).includes(id as ProgramId);
};

export const isValidActionType = (action: string): action is ActionType => {
  return Object.values(ActionType).includes(action as ActionType);
};

// Utility Types
export type DeepPartial<T> = {
  [P in keyof T]?: T[P] extends object ? DeepPartial<T[P]> : T[P];
};

export type RequiredFields<T, K extends keyof T> = T & Required<Pick<T, K>>;

export type OptionalFields<T, K extends keyof T> = Omit<T, K> & Partial<Pick<T, K>>;

export type FormDataFor<T extends ProgramId> = 
  T extends ProgramId.COUSR01C ? CreateUserFormData :
  T extends ProgramId.COUSR02C ? UpdateUserFormData :
  T extends ProgramId.COUSR00C ? UserFilterCriteria :
  never;

export type StateFor<T extends ProgramId> = 
  T extends ProgramId.COADM01C ? COADM01CState :
  T extends ProgramId.COUSR00C ? COUSR00CState :
  T extends ProgramId.COUSR01C ? COUSR01CState :
  T extends ProgramId.COUSR02C ? COUSR02CState :
  T extends ProgramId.COUSR03C ? COUSR03CState :
  never;