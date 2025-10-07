// src/services/userSecurityService.ts

import {
  SignonRequestDTO,
  SignonResponseDTO,
  UserSecurityDTO,
  CreateUserSecurityRequest,
  UpdateUserSecurityRequest,
  ChangePasswordRequest,
  ValidationResponseDTO,
  UserSecurityListResponse,
  UserSecurityApiResponse,
  UserSecurityFilters,
  SessionInfo,
  UserSecurityActionResult,
  ValidationError
} from '@/types/userSecurity';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || '';

class UserSecurityService {
  private async makeRequest<T>(
    endpoint: string,
    options: RequestInit = {}
  ): Promise<UserSecurityApiResponse<T>> {
    try {
      const url = `${API_BASE_URL}${endpoint}`;
      const defaultHeaders = {
        'Content-Type': 'application/json',
        ...options.headers,
      };

      // Add authorization header if session token exists
      const sessionToken = this.getSessionToken();
      if (sessionToken) {
        defaultHeaders['Authorization'] = `Bearer ${sessionToken}`;
      }

      const response = await fetch(url, {
        ...options,
        headers: defaultHeaders,
      });

      const data = await response.json();

      if (!response.ok) {
        return {
          success: false,
          error: {
            code: data.errorCode || 'HTTP_ERROR',
            message: data.errorMessage || `HTTP ${response.status}: ${response.statusText}`,
            details: data.details || undefined,
          },
          timestamp: new Date().toISOString(),
        };
      }

      return {
        success: true,
        data,
        timestamp: new Date().toISOString(),
      };
    } catch (error) {
      return {
        success: false,
        error: {
          code: 'NETWORK_ERROR',
          message: error instanceof Error ? error.message : 'Network request failed',
          details: 'Please check your internet connection and try again.',
        },
        timestamp: new Date().toISOString(),
      };
    }
  }

  private getSessionToken(): string | null {
    if (typeof window !== 'undefined') {
      return localStorage.getItem('sessionToken') || sessionStorage.getItem('sessionToken');
    }
    return null;
  }

  private setSessionToken(token: string, remember: boolean = false): void {
    if (typeof window !== 'undefined') {
      if (remember) {
        localStorage.setItem('sessionToken', token);
      } else {
        sessionStorage.setItem('sessionToken', token);
      }
    }
  }

  private removeSessionToken(): void {
    if (typeof window !== 'undefined') {
      localStorage.removeItem('sessionToken');
      sessionStorage.removeItem('sessionToken');
    }
  }

  // Authentication Methods
  async signon(
    request: SignonRequestDTO,
    rememberMe: boolean = false
  ): Promise<UserSecurityApiResponse<SignonResponseDTO>> {
    const response = await this.makeRequest<SignonResponseDTO>('/api/auth/signon', {
      method: 'POST',
      body: JSON.stringify(request),
    });

    if (response.success && response.data?.sessionToken) {
      this.setSessionToken(response.data.sessionToken, rememberMe);
    }

    return response;
  }

  async exit(): Promise<UserSecurityApiResponse<void>> {
    const response = await this.makeRequest<void>('/api/auth/exit', {
      method: 'POST',
    });

    // Always remove session token on exit, regardless of API response
    this.removeSessionToken();

    return response;
  }

  async invalidateKey(): Promise<UserSecurityApiResponse<void>> {
    const response = await this.makeRequest<void>('/api/auth/invalid-key', {
      method: 'POST',
    });

    if (response.success) {
      this.removeSessionToken();
    }

    return response;
  }

  async validate(): Promise<UserSecurityApiResponse<ValidationResponseDTO>> {
    return this.makeRequest<ValidationResponseDTO>('/api/auth/validate', {
      method: 'POST',
    });
  }

  // User Management Methods
  async getUsers(filters?: UserSecurityFilters): Promise<UserSecurityApiResponse<UserSecurityListResponse>> {
    const queryParams = new URLSearchParams();

    if (filters) {
      Object.entries(filters).forEach(([key, value]) => {
        if (value !== undefined && value !== null && value !== '') {
          queryParams.append(key, value.toString());
        }
      });
    }

    const queryString = queryParams.toString();
    const endpoint = queryString ? `/api/users?${queryString}` : '/api/users';

    return this.makeRequest<UserSecurityListResponse>(endpoint, {
      method: 'GET',
    });
  }

  async getUserById(userId: string): Promise<UserSecurityApiResponse<UserSecurityDTO>> {
    if (!userId) {
      return {
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'User ID is required',
        },
        timestamp: new Date().toISOString(),
      };
    }

    return this.makeRequest<UserSecurityDTO>(`/api/users/${encodeURIComponent(userId)}`, {
      method: 'GET',
    });
  }

  async createUser(request: CreateUserSecurityRequest): Promise<UserSecurityApiResponse<UserSecurityDTO>> {
    const validationErrors = this.validateCreateUserRequest(request);
    if (validationErrors.length > 0) {
      return {
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'Validation failed',
          details: validationErrors.map(e => `${e.field}: ${e.message}`).join(', '),
        },
        timestamp: new Date().toISOString(),
      };
    }

    return this.makeRequest<UserSecurityDTO>('/api/users', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  }

  async updateUser(
    userId: string,
    request: UpdateUserSecurityRequest
  ): Promise<UserSecurityApiResponse<UserSecurityDTO>> {
    if (!userId) {
      return {
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'User ID is required',
        },
        timestamp: new Date().toISOString(),
      };
    }

    const validationErrors = this.validateUpdateUserRequest(request);
    if (validationErrors.length > 0) {
      return {
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'Validation failed',
          details: validationErrors.map(e => `${e.field}: ${e.message}`).join(', '),
        },
        timestamp: new Date().toISOString(),
      };
    }

    return this.makeRequest<UserSecurityDTO>(`/api/users/${encodeURIComponent(userId)}`, {
      method: 'PUT',
      body: JSON.stringify(request),
    });
  }

  async deleteUser(userId: string): Promise<UserSecurityApiResponse<void>> {
    if (!userId) {
      return {
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'User ID is required',
        },
        timestamp: new Date().toISOString(),
      };
    }

    return this.makeRequest<void>(`/api/users/${encodeURIComponent(userId)}`, {
      method: 'DELETE',
    });
  }

  // User Action Methods
  async activateUser(userId: string): Promise<UserSecurityApiResponse<UserSecurityDTO>> {
    if (!userId) {
      return {
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'User ID is required',
        },
        timestamp: new Date().toISOString(),
      };
    }

    return this.makeRequest<UserSecurityDTO>(`/api/users/${encodeURIComponent(userId)}/activate`, {
      method: 'PATCH',
    });
  }

  async deactivateUser(userId: string): Promise<UserSecurityApiResponse<UserSecurityDTO>> {
    if (!userId) {
      return {
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'User ID is required',
        },
        timestamp: new Date().toISOString(),
      };
    }

    return this.makeRequest<UserSecurityDTO>(`/api/users/${encodeURIComponent(userId)}/deactivate`, {
      method: 'PATCH',
    });
  }

  async changePassword(
    userId: string,
    request: ChangePasswordRequest
  ): Promise<UserSecurityApiResponse<void>> {
    if (!userId) {
      return {
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'User ID is required',
        },
        timestamp: new Date().toISOString(),
      };
    }

    const validationErrors = this.validatePasswordChangeRequest(request);
    if (validationErrors.length > 0) {
      return {
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'Validation failed',
          details: validationErrors.map(e => `${e.field}: ${e.message}`).join(', '),
        },
        timestamp: new Date().toISOString(),
      };
    }

    return this.makeRequest<void>(`/api/users/${encodeURIComponent(userId)}/change-password`, {
      method: 'PATCH',
      body: JSON.stringify(request),
    });
  }

  // Utility Methods
  getCurrentSession(): SessionInfo | null {
    const token = this.getSessionToken();
    if (!token) return null;

    try {
      // Decode JWT token to get session info (basic implementation)
      const payload = JSON.parse(atob(token.split('.')[1]));
      return {
        sessionToken: token,
        userId: payload.userId,
        userName: payload.userName,
        userType: payload.userType,
        isActive: payload.isActive,
        createdAt: new Date(payload.iat * 1000),
        expiresAt: new Date(payload.exp * 1000),
        lastActivity: new Date(),
      };
    } catch {
      this.removeSessionToken();
      return null;
    }
  }

  isAuthenticated(): boolean {
    const session = this.getCurrentSession();
    return session !== null && session.expiresAt > new Date();
  }

  // Validation Methods
  private validateCreateUserRequest(request: CreateUserSecurityRequest): ValidationError[] {
    const errors: ValidationError[] = [];

    if (!request.userId?.trim()) {
      errors.push({
        field: 'userId',
        message: 'User ID is required',
        code: 'REQUIRED',
      });
    } else if (request.userId.length > 8) {
      errors.push({
        field: 'userId',
        message: 'User ID cannot exceed 8 characters',
        code: 'LENGTH',
      });
    }

    if (!request.firstName?.trim()) {
      errors.push({
        field: 'firstName',
        message: 'First name is required',
        code: 'REQUIRED',
      });
    }

    if (!request.lastName?.trim()) {
      errors.push({
        field: 'lastName',
        message: 'Last name is required',
        code: 'REQUIRED',
      });
    }

    if (!request.password) {
      errors.push({
        field: 'password',
        message: 'Password is required',
        code: 'REQUIRED',
      });
    } else if (request.password.length > 8) {
      errors.push({
        field: 'password',
        message: 'Password cannot exceed 8 characters',
        code: 'LENGTH',
      });
    }

    if (!request.userType?.trim()) {
      errors.push({
        field: 'userType',
        message: 'User Type is required',
        code: 'REQUIRED',
      });
    }

    if (request.password !== request.confirmPassword) {
      errors.push({
        field: 'confirmPassword',
        message: 'Passwords do not match',
        code: 'MISMATCH',
      });
    }

    return errors;
  }

  private validateUpdateUserRequest(request: UpdateUserSecurityRequest): ValidationError[] {
    const errors: ValidationError[] = [];

    if (request.firstName !== undefined && (!request.firstName || request.firstName.trim() === '')) {
      errors.push({
        field: 'firstName',
        message: 'First Name can NOT be empty...',
        code: 'REQUIRED',
      });
    }

    if (request.lastName !== undefined && (!request.lastName || request.lastName.trim() === '')) {
      errors.push({
        field: 'lastName',
        message: 'Last Name can NOT be empty...',
        code: 'REQUIRED',
      });
    }

    if (request.userType !== undefined && (!request.userType || request.userType.trim() === '')) {
      errors.push({
        field: 'userType',
        message: 'User Type can NOT be empty...',
        code: 'REQUIRED',
      });
    }

    return errors;
  }

  private validatePasswordChangeRequest(request: ChangePasswordRequest): ValidationError[] {
    const errors: ValidationError[] = [];

    if (!request.newPassword) {
      errors.push({
        field: 'newPassword',
        message: 'New password is required',
        code: 'REQUIRED',
      });
    } else if (request.newPassword.length > 8) {
      errors.push({
        field: 'newPassword',
        message: 'Password cannot exceed 8 characters',
        code: 'LENGTH',
      });
    }

    if (request.newPassword !== request.confirmPassword) {
      errors.push({
        field: 'confirmPassword',
        message: 'Passwords do not match',
        code: 'MISMATCH',
      });
    }

    return errors;
  }

  // Helper method to convert API response to action result
  private toActionResult<T>(
    response: UserSecurityApiResponse<T>,
    successMessage: string
  ): UserSecurityActionResult {
    if (response.success) {
      return {
        success: true,
        message: successMessage,
        data: response.data as UserSecurityDTO,
      };
    } else {
      return {
        success: false,
        message: response.error?.message || 'Operation failed',
        errors: response.error?.details
          ? [{ field: 'general', message: response.error.details, code: response.error.code }]
          : undefined,
      };
    }
  }

  // Convenience methods that return UserSecurityActionResult
  async createUserWithResult(request: CreateUserSecurityRequest): Promise<UserSecurityActionResult> {
    const response = await this.createUser(request);
    return this.toActionResult(response, 'User created successfully');
  }

  async updateUserWithResult(
    userId: string,
    request: UpdateUserSecurityRequest
  ): Promise<UserSecurityActionResult> {
    const response = await this.updateUser(userId, request);
    return this.toActionResult(response, 'User updated successfully');
  }

  async deleteUserWithResult(userId: string): Promise<UserSecurityActionResult> {
    const response = await this.deleteUser(userId);
    return {
      success: response.success,
      message: response.success ? 'User deleted successfully' : response.error?.message || 'Delete failed',
      errors: response.error?.details
        ? [{ field: 'general', message: response.error.details, code: response.error.code }]
        : undefined,
    };
  }

  async activateUserWithResult(userId: string): Promise<UserSecurityActionResult> {
    const response = await this.activateUser(userId);
    return this.toActionResult(response, 'User activated successfully');
  }

  async deactivateUserWithResult(userId: string): Promise<UserSecurityActionResult> {
    const response = await this.deactivateUser(userId);
    return this.toActionResult(response, 'User deactivated successfully');
  }

  async changePasswordWithResult(
    userId: string,
    request: ChangePasswordRequest
  ): Promise<UserSecurityActionResult> {
    const response = await this.changePassword(userId, request);
    return {
      success: response.success,
      message: response.success ? 'Password changed successfully' : response.error?.message || 'Password change failed',
      errors: response.error?.details
        ? [{ field: 'general', message: response.error.details, code: response.error.code }]
        : undefined,
    };
  }
}

// Export singleton instance
export const userSecurityService = new UserSecurityService();
export default userSecurityService;