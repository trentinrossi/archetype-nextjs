// src/services/userService.ts

import { 
  User, 
  CreateUserRequest, 
  UpdateUserRequest, 
  SignonRequest, 
  SignonResponse, 
  AuthUser, 
  UserValidationError, 
  UserValidationResult, 
  PaginationParams, 
  PaginatedUsersResponse, 
  APIError, 
  APIResponse, 
  UserFilters, 
  UserSearchParams, 
  UserStats, 
  PasswordChangeRequest, 
  UserSession, 
  RefreshTokenRequest, 
  RefreshTokenResponse,
  USER_VALIDATION_RULES,
  DEFAULT_PAGINATION,
  MAX_PAGINATION_LIMIT,
  getUserRole
} from '@/types/user';

const API_BASE_URL = '/api';

class UserService {
  private currentSession: UserSession | null = null;

  private getAuthHeaders(): Record<string, string> {
    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
    };

    if (this.currentSession?.token) {
      headers['Authorization'] = `Bearer ${this.currentSession.token}`;
    }

    return headers;
  }

  private async handleResponse<T>(response: Response): Promise<APIResponse<T>> {
    let data: any;
    
    try {
      data = await response.json();
    } catch (error) {
      data = {};
    }

    if (!response.ok) {
      const apiError: APIError = {
        error: data.error || `HTTP ${response.status}`,
        message: data.message || response.statusText,
        details: data.details,
        statusCode: response.status,
      };

      return {
        success: false,
        error: apiError,
        message: apiError.message,
      };
    }

    return {
      success: true,
      data: data,
      message: data.message,
    };
  }

  private handleKeyboardEvent(event: KeyboardEvent): void {
    switch (event.key) {
      case 'Enter':
        // Handle authentication submission
        const activeElement = document.activeElement as HTMLElement;
        if (activeElement?.tagName === 'BUTTON' || activeElement?.type === 'submit') {
          activeElement.click();
        }
        break;
      case 'F3':
        event.preventDefault();
        this.handleExit();
        break;
    }
  }

  private handleExit(): void {
    if (typeof window !== 'undefined') {
      const confirmExit = window.confirm('Are you sure you want to exit?');
      if (confirmExit) {
        this.signout();
        window.location.href = '/';
      }
    }
  }

  public initializeKeyboardHandlers(): void {
    if (typeof window !== 'undefined') {
      document.addEventListener('keydown', this.handleKeyboardEvent.bind(this));
    }
  }

  public removeKeyboardHandlers(): void {
    if (typeof window !== 'undefined') {
      document.removeEventListener('keydown', this.handleKeyboardEvent.bind(this));
    }
  }

  public validateUser(user: Partial<User>): UserValidationResult {
    const errors: UserValidationError[] = [];

    // Validate User ID
    if (user.userId) {
      if (user.userId.length !== USER_VALIDATION_RULES.USER_ID_LENGTH) {
        errors.push({
          field: 'userId',
          message: `User ID must be exactly ${USER_VALIDATION_RULES.USER_ID_LENGTH} characters`,
        });
      }
      if (!/^[A-Za-z0-9]+$/.test(user.userId)) {
        errors.push({
          field: 'userId',
          message: 'User ID must contain only alphanumeric characters',
        });
      }
    }

    // Validate Password
    if (user.password) {
      if (user.password.length !== USER_VALIDATION_RULES.PASSWORD_LENGTH) {
        errors.push({
          field: 'password',
          message: `Password must be exactly ${USER_VALIDATION_RULES.PASSWORD_LENGTH} characters`,
        });
      }
    }

    // Validate First Name
    if (user.firstName) {
      if (user.firstName.length > USER_VALIDATION_RULES.FIRST_NAME_MAX_LENGTH) {
        errors.push({
          field: 'firstName',
          message: `First name cannot exceed ${USER_VALIDATION_RULES.FIRST_NAME_MAX_LENGTH} characters`,
        });
      }
      if (user.firstName.trim().length === 0) {
        errors.push({
          field: 'firstName',
          message: 'First name is required',
        });
      }
    }

    // Validate Last Name
    if (user.lastName) {
      if (user.lastName.length > USER_VALIDATION_RULES.LAST_NAME_MAX_LENGTH) {
        errors.push({
          field: 'lastName',
          message: `Last name cannot exceed ${USER_VALIDATION_RULES.LAST_NAME_MAX_LENGTH} characters`,
        });
      }
      if (user.lastName.trim().length === 0) {
        errors.push({
          field: 'lastName',
          message: 'Last name is required',
        });
      }
    }

    // Validate User Type
    if (user.userType && !USER_VALIDATION_RULES.VALID_USER_TYPES.includes(user.userType)) {
      errors.push({
        field: 'userType',
        message: 'User type must be A (Admin) or U (User)',
      });
    }

    return {
      isValid: errors.length === 0,
      errors,
    };
  }

  public validateSignonRequest(request: SignonRequest): UserValidationResult {
    const errors: UserValidationError[] = [];

    if (!request.userId || request.userId.trim().length === 0) {
      errors.push({
        field: 'userId',
        message: 'User ID is required',
      });
    } else if (request.userId.length !== USER_VALIDATION_RULES.USER_ID_LENGTH) {
      errors.push({
        field: 'userId',
        message: `User ID must be exactly ${USER_VALIDATION_RULES.USER_ID_LENGTH} characters`,
      });
    }

    if (!request.password || request.password.length === 0) {
      errors.push({
        field: 'password',
        message: 'Password is required',
      });
    } else if (request.password.length !== USER_VALIDATION_RULES.PASSWORD_LENGTH) {
      errors.push({
        field: 'password',
        message: `Password must be exactly ${USER_VALIDATION_RULES.PASSWORD_LENGTH} characters`,
      });
    }

    return {
      isValid: errors.length === 0,
      errors,
    };
  }

  private validatePaginationParams(params: Partial<PaginationParams>): PaginationParams {
    return {
      page: Math.max(1, params.page || DEFAULT_PAGINATION.page),
      limit: Math.min(MAX_PAGINATION_LIMIT, Math.max(1, params.limit || DEFAULT_PAGINATION.limit)),
      sortBy: params.sortBy || DEFAULT_PAGINATION.sortBy,
      sortOrder: params.sortOrder || DEFAULT_PAGINATION.sortOrder,
    };
  }

  public async signon(request: SignonRequest): Promise<APIResponse<SignonResponse>> {
    try {
      const validation = this.validateSignonRequest(request);
      if (!validation.isValid) {
        return {
          success: false,
          error: {
            error: 'Validation Error',
            message: 'Invalid credentials format',
            details: validation.errors.map(e => e.message).join(', '),
            statusCode: 400,
          },
        };
      }

      const response = await fetch(`${API_BASE_URL}/auth/login`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(request),
      });

      const result = await this.handleResponse<SignonResponse>(response);

      if (result.success && result.data?.success && result.data.token) {
        this.currentSession = {
          userId: result.data.user!.userId,
          token: result.data.token,
          expiresAt: new Date(Date.now() + 24 * 60 * 60 * 1000).toISOString(), // 24 hours
        };

        if (typeof window !== 'undefined') {
          localStorage.setItem('userSession', JSON.stringify(this.currentSession));
        }
      }

      return result;
    } catch (error) {
      return {
        success: false,
        error: {
          error: 'Network Error',
          message: 'Failed to connect to authentication service',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public async signout(): Promise<APIResponse<void>> {
    try {
      const response = await fetch(`${API_BASE_URL}/auth/logout`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<void>(response);

      // Clear session regardless of API response
      this.currentSession = null;
      if (typeof window !== 'undefined') {
        localStorage.removeItem('userSession');
      }

      return result;
    } catch (error) {
      // Clear session even on error
      this.currentSession = null;
      if (typeof window !== 'undefined') {
        localStorage.removeItem('userSession');
      }

      return {
        success: false,
        error: {
          error: 'Network Error',
          message: 'Failed to sign out properly',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public async validateSession(): Promise<APIResponse<AuthUser>> {
    try {
      if (!this.currentSession) {
        if (typeof window !== 'undefined') {
          const stored = localStorage.getItem('userSession');
          if (stored) {
            this.currentSession = JSON.parse(stored);
          }
        }
      }

      if (!this.currentSession) {
        return {
          success: false,
          error: {
            error: 'No Session',
            message: 'No active session found',
            statusCode: 401,
          },
        };
      }

      // Check if session is expired
      if (new Date() > new Date(this.currentSession.expiresAt)) {
        this.currentSession = null;
        if (typeof window !== 'undefined') {
          localStorage.removeItem('userSession');
        }
        return {
          success: false,
          error: {
            error: 'Session Expired',
            message: 'Session has expired',
            statusCode: 401,
          },
        };
      }

      const response = await fetch(`${API_BASE_URL}/auth/validate`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<AuthUser>(response);
    } catch (error) {
      return {
        success: false,
        error: {
          error: 'Validation Error',
          message: 'Failed to validate session',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public async listUsers(params: Partial<UserSearchParams> = {}): Promise<APIResponse<PaginatedUsersResponse>> {
    try {
      const validatedParams = this.validatePaginationParams(params);
      const queryParams = new URLSearchParams();

      queryParams.append('page', validatedParams.page.toString());
      queryParams.append('limit', validatedParams.limit.toString());
      queryParams.append('sortBy', validatedParams.sortBy || 'userId');
      queryParams.append('sortOrder', validatedParams.sortOrder || 'asc');

      if (params.filters?.userType) {
        queryParams.append('userType', params.filters.userType);
      }
      if (params.filters?.search) {
        queryParams.append('search', params.filters.search);
      }

      const response = await fetch(`${API_BASE_URL}/users?${queryParams.toString()}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<PaginatedUsersResponse>(response);
    } catch (error) {
      return {
        success: false,
        error: {
          error: 'Fetch Error',
          message: 'Failed to fetch users',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public async getUserById(userId: string): Promise<APIResponse<User>> {
    try {
      if (!userId || userId.trim().length === 0) {
        return {
          success: false,
          error: {
            error: 'Invalid Input',
            message: 'User ID is required',
            statusCode: 400,
          },
        };
      }

      const response = await fetch(`${API_BASE_URL}/users/${userId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<User>(response);
    } catch (error) {
      return {
        success: false,
        error: {
          error: 'Fetch Error',
          message: 'Failed to fetch user',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public async createUser(userData: CreateUserRequest): Promise<APIResponse<User>> {
    try {
      const validation = this.validateUser(userData as Partial<User>);
      if (!validation.isValid) {
        return {
          success: false,
          error: {
            error: 'Validation Error',
            message: 'Invalid user data',
            details: validation.errors.map(e => e.message).join(', '),
            statusCode: 400,
          },
        };
      }

      const response = await fetch(`${API_BASE_URL}/users`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(userData),
      });

      return await this.handleResponse<User>(response);
    } catch (error) {
      return {
        success: false,
        error: {
          error: 'Create Error',
          message: 'Failed to create user',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public async updateUser(userId: string, userData: UpdateUserRequest): Promise<APIResponse<User>> {
    try {
      if (!userId || userId.trim().length === 0) {
        return {
          success: false,
          error: {
            error: 'Invalid Input',
            message: 'User ID is required',
            statusCode: 400,
          },
        };
      }

      const validation = this.validateUser(userData as Partial<User>);
      if (!validation.isValid) {
        return {
          success: false,
          error: {
            error: 'Validation Error',
            message: 'Invalid user data',
            details: validation.errors.map(e => e.message).join(', '),
            statusCode: 400,
          },
        };
      }

      const response = await fetch(`${API_BASE_URL}/users/${userId}`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(userData),
      });

      return await this.handleResponse<User>(response);
    } catch (error) {
      return {
        success: false,
        error: {
          error: 'Update Error',
          message: 'Failed to update user',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public async deleteUser(userId: string): Promise<APIResponse<void>> {
    try {
      if (!userId || userId.trim().length === 0) {
        return {
          success: false,
          error: {
            error: 'Invalid Input',
            message: 'User ID is required',
            statusCode: 400,
          },
        };
      }

      const response = await fetch(`${API_BASE_URL}/users/${userId}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<void>(response);
    } catch (error) {
      return {
        success: false,
        error: {
          error: 'Delete Error',
          message: 'Failed to delete user',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public async changePassword(userId: string, passwordData: PasswordChangeRequest): Promise<APIResponse<void>> {
    try {
      if (!userId || userId.trim().length === 0) {
        return {
          success: false,
          error: {
            error: 'Invalid Input',
            message: 'User ID is required',
            statusCode: 400,
          },
        };
      }

      if (passwordData.newPassword !== passwordData.confirmPassword) {
        return {
          success: false,
          error: {
            error: 'Validation Error',
            message: 'New password and confirmation do not match',
            statusCode: 400,
          },
        };
      }

      if (passwordData.newPassword.length !== USER_VALIDATION_RULES.PASSWORD_LENGTH) {
        return {
          success: false,
          error: {
            error: 'Validation Error',
            message: `Password must be exactly ${USER_VALIDATION_RULES.PASSWORD_LENGTH} characters`,
            statusCode: 400,
          },
        };
      }

      const response = await fetch(`${API_BASE_URL}/users/${userId}/password`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify({
          currentPassword: passwordData.currentPassword,
          newPassword: passwordData.newPassword,
        }),
      });

      return await this.handleResponse<void>(response);
    } catch (error) {
      return {
        success: false,
        error: {
          error: 'Password Change Error',
          message: 'Failed to change password',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public async getUserStats(): Promise<APIResponse<UserStats>> {
    try {
      const response = await fetch(`${API_BASE_URL}/users/stats`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<UserStats>(response);
    } catch (error) {
      return {
        success: false,
        error: {
          error: 'Stats Error',
          message: 'Failed to fetch user statistics',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public async refreshToken(refreshTokenData: RefreshTokenRequest): Promise<APIResponse<RefreshTokenResponse>> {
    try {
      const response = await fetch(`${API_BASE_URL}/auth/refresh`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(refreshTokenData),
      });

      const result = await this.handleResponse<RefreshTokenResponse>(response);

      if (result.success && result.data) {
        this.currentSession = {
          userId: this.currentSession?.userId || '',
          token: result.data.token,
          expiresAt: result.data.expiresAt,
          refreshToken: result.data.refreshToken,
        };

        if (typeof window !== 'undefined') {
          localStorage.setItem('userSession', JSON.stringify(this.currentSession));
        }
      }

      return result;
    } catch (error) {
      return {
        success: false,
        error: {
          error: 'Token Refresh Error',
          message: 'Failed to refresh authentication token',
          details: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 500,
        },
      };
    }
  }

  public getCurrentSession(): UserSession | null {
    return this.currentSession;
  }

  public isAuthenticated(): boolean {
    if (!this.currentSession) {
      if (typeof window !== 'undefined') {
        const stored = localStorage.getItem('userSession');
        if (stored) {
          this.currentSession = JSON.parse(stored);
        }
      }
    }

    if (!this.currentSession) {
      return false;
    }

    return new Date() <= new Date(this.currentSession.expiresAt);
  }

  public generateUserId(): string {
    const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
    let result = '';
    for (let i = 0; i < USER_VALIDATION_RULES.USER_ID_LENGTH; i++) {
      result += chars.charAt(Math.floor(Math.random() * chars.length));
    }
    return result;
  }

  public generatePassword(): string {
    const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    let result = '';
    for (let i = 0; i < USER_VALIDATION_RULES.PASSWORD_LENGTH; i++) {
      result += chars.charAt(Math.floor(Math.random() * chars.length));
    }
    return result;
  }
}

export const userService = new UserService();