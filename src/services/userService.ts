// src/services/userService.ts

import {
  User,
  UserType,
  CreateUserRequest,
  UpdateUserRequest,
  UserLoginRequest,
  ChangePasswordRequest,
  UserResponse,
  UserListResponse,
  UserLoginResponse,
  UserCreateResponse,
  UserUpdateResponse,
  UserDeleteResponse,
  PaginationRequest,
  UserSearchRequest,
  APIError,
  APIErrorResponse,
  DEFAULT_PAGINATION,
} from '@/types/user';

const API_BASE_URL = '/api';

class UserService {
  private getAuthHeaders(): Record<string, string> {
    const token = typeof window !== 'undefined' ? localStorage.getItem('authToken') : null;
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  private async handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
      let errorData: APIErrorResponse;
      try {
        errorData = await response.json();
      } catch {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      const errorMessage = errorData.error?.message || 
                          errorData.error?.details?.[0] || 
                          `HTTP ${response.status}: ${response.statusText}`;
      
      throw new Error(errorMessage);
    }

    const contentType = response.headers.get('content-type');
    if (contentType && contentType.includes('application/json')) {
      return response.json();
    }
    
    return {} as T;
  }

  private buildQueryString(params: Record<string, any>): string {
    const searchParams = new URLSearchParams();
    
    Object.entries(params).forEach(([key, value]) => {
      if (value !== undefined && value !== null && value !== '') {
        searchParams.append(key, String(value));
      }
    });
    
    return searchParams.toString();
  }

  private validateUserId(userId: string): void {
    if (!userId || userId.length === 0) {
      throw new Error('User ID is required');
    }
    if (userId.length > 8) {
      throw new Error('User ID must be 8 characters or less');
    }
    if (!/^[A-Z0-9]+$/i.test(userId)) {
      throw new Error('User ID must contain only alphanumeric characters');
    }
  }

  private validateUserData(userData: CreateUserRequest | UpdateUserRequest): void {
    if ('userId' in userData) {
      this.validateUserId(userData.userId);
    }
    
    if (userData.firstName && userData.firstName.length > 20) {
      throw new Error('First name must be 20 characters or less');
    }
    
    if (userData.lastName && userData.lastName.length > 20) {
      throw new Error('Last name must be 20 characters or less');
    }
    
    if (userData.password && userData.password.length !== 8) {
      throw new Error('Password must be exactly 8 characters');
    }
    
    if (userData.userType && !Object.values(UserType).includes(userData.userType)) {
      throw new Error('Invalid user type');
    }
  }

  // COSGN00C - User Authentication
  async authenticateUser(credentials: UserLoginRequest): Promise<UserLoginResponse> {
    try {
      this.validateUserId(credentials.userId);
      
      if (!credentials.password || credentials.password.length !== 8) {
        throw new Error('Password must be exactly 8 characters');
      }

      const response = await fetch(`${API_BASE_URL}/users/signin`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          userId: credentials.userId.toUpperCase(),
          password: credentials.password,
        }),
      });

      const result = await this.handleResponse<UserLoginResponse>(response);
      
      // Store token in localStorage for subsequent requests
      if (typeof window !== 'undefined' && result.token) {
        localStorage.setItem('authToken', result.token);
        localStorage.setItem('userSession', JSON.stringify(result));
      }

      return result;
    } catch (error) {
      throw new Error(error instanceof Error ? error.message : 'Authentication failed');
    }
  }

  // User logout
  async logout(): Promise<void> {
    try {
      await fetch(`${API_BASE_URL}/auth/logout`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });
    } catch (error) {
      // Continue with logout even if API call fails
      console.warn('Logout API call failed:', error);
    } finally {
      // Always clear local storage
      if (typeof window !== 'undefined') {
        localStorage.removeItem('authToken');
        localStorage.removeItem('userSession');
      }
    }
  }

  // Change password
  async changePassword(changePasswordData: ChangePasswordRequest): Promise<void> {
    try {
      this.validateUserId(changePasswordData.userId);
      
      if (!changePasswordData.currentPassword || changePasswordData.currentPassword.length !== 8) {
        throw new Error('Current password must be exactly 8 characters');
      }
      
      if (!changePasswordData.newPassword || changePasswordData.newPassword.length !== 8) {
        throw new Error('New password must be exactly 8 characters');
      }

      const response = await fetch(`${API_BASE_URL}/auth/change-password`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify({
          userId: changePasswordData.userId.toUpperCase(),
          currentPassword: changePasswordData.currentPassword,
          newPassword: changePasswordData.newPassword,
        }),
      });

      await this.handleResponse<void>(response);
    } catch (error) {
      throw new Error(error instanceof Error ? error.message : 'Password change failed');
    }
  }

  // COUSR00C - User Listing with Pagination
  async listUsers(searchParams: UserSearchRequest = {}): Promise<UserListResponse> {
    try {
      const params = {
        ...DEFAULT_PAGINATION,
        ...searchParams,
      };

      // Validate pagination parameters
      if (params.page && params.page < 1) {
        params.page = 1;
      }
      
      if (params.limit && (params.limit < 1 || params.limit > 100)) {
        params.limit = DEFAULT_PAGINATION.limit;
      }

      const queryString = this.buildQueryString(params);
      const url = `${API_BASE_URL}/users${queryString ? `?${queryString}` : ''}`;

      const response = await fetch(url, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      const data = await this.handleResponse<UserListResponse>(response);
      
      // Ensure we always return a valid structure
      return {
        users: Array.isArray(data.users) ? data.users : [],
        pagination: data.pagination || {
          currentPage: params.page || 1,
          totalPages: 1,
          totalItems: 0,
          itemsPerPage: params.limit || 10,
          hasNextPage: false,
          hasPreviousPage: false,
        },
      };
    } catch (error) {
      throw new Error(error instanceof Error ? error.message : 'Failed to fetch users');
    }
  }

  // Get user by ID
  async getUserById(userId: string): Promise<UserResponse> {
    try {
      this.validateUserId(userId);

      const response = await fetch(`${API_BASE_URL}/users/${userId.toUpperCase()}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<UserResponse>(response);
    } catch (error) {
      throw new Error(error instanceof Error ? error.message : 'Failed to fetch user');
    }
  }

  // Search users by criteria
  async searchUsers(searchTerm: string, searchParams: UserSearchRequest = {}): Promise<UserListResponse> {
    try {
      const params = {
        ...DEFAULT_PAGINATION,
        ...searchParams,
        searchTerm: searchTerm.trim(),
      };

      return await this.listUsers(params);
    } catch (error) {
      throw new Error(error instanceof Error ? error.message : 'Failed to search users');
    }
  }

  // COUSR01C - User Creation
  async createUser(userData: CreateUserRequest): Promise<UserCreateResponse> {
    try {
      this.validateUserData(userData);

      const response = await fetch(`${API_BASE_URL}/users`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify({
          userId: userData.userId.toUpperCase(),
          firstName: userData.firstName.trim(),
          lastName: userData.lastName.trim(),
          password: userData.password,
          userType: userData.userType,
        }),
      });

      return await this.handleResponse<UserCreateResponse>(response);
    } catch (error) {
      throw new Error(error instanceof Error ? error.message : 'Failed to create user');
    }
  }

  // COUSR02C - User Update
  async updateUser(userId: string, userData: UpdateUserRequest): Promise<UserUpdateResponse> {
    try {
      this.validateUserId(userId);
      this.validateUserData(userData);

      // Remove undefined fields
      const updateData = Object.entries(userData).reduce((acc, [key, value]) => {
        if (value !== undefined) {
          if (key === 'firstName' || key === 'lastName') {
            acc[key] = (value as string).trim();
          } else {
            acc[key] = value;
          }
        }
        return acc;
      }, {} as Record<string, any>);

      const response = await fetch(`${API_BASE_URL}/users/${userId.toUpperCase()}`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(updateData),
      });

      return await this.handleResponse<UserUpdateResponse>(response);
    } catch (error) {
      throw new Error(error instanceof Error ? error.message : 'Failed to update user');
    }
  }

  // Partial user update (PATCH)
  async patchUser(userId: string, userData: Partial<UpdateUserRequest>): Promise<UserUpdateResponse> {
    try {
      this.validateUserId(userId);
      
      if (Object.keys(userData).length === 0) {
        throw new Error('No data provided for update');
      }

      this.validateUserData(userData as UpdateUserRequest);

      const response = await fetch(`${API_BASE_URL}/users/${userId.toUpperCase()}`, {
        method: 'PATCH',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(userData),
      });

      return await this.handleResponse<UserUpdateResponse>(response);
    } catch (error) {
      throw new Error(error instanceof Error ? error.message : 'Failed to update user');
    }
  }

  // COUSR03C - User Deletion
  async deleteUser(userId: string): Promise<UserDeleteResponse> {
    try {
      this.validateUserId(userId);

      const response = await fetch(`${API_BASE_URL}/users/${userId.toUpperCase()}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<UserDeleteResponse>(response);
    } catch (error) {
      throw new Error(error instanceof Error ? error.message : 'Failed to delete user');
    }
  }

  // Bulk operations
  async bulkDeleteUsers(userIds: string[]): Promise<{ deleted: string[]; failed: string[] }> {
    try {
      if (!Array.isArray(userIds) || userIds.length === 0) {
        throw new Error('User IDs array is required');
      }

      // Validate all user IDs
      userIds.forEach(userId => this.validateUserId(userId));

      const response = await fetch(`${API_BASE_URL}/users/bulk-delete`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
        body: JSON.stringify({
          userIds: userIds.map(id => id.toUpperCase()),
        }),
      });

      return await this.handleResponse<{ deleted: string[]; failed: string[] }>(response);
    } catch (error) {
      throw new Error(error instanceof Error ? error.message : 'Failed to delete users');
    }
  }

  // User existence check
  async checkUserExists(userId: string): Promise<boolean> {
    try {
      this.validateUserId(userId);

      const response = await fetch(`${API_BASE_URL}/users/${userId.toUpperCase()}/exists`, {
        method: 'HEAD',
        headers: this.getAuthHeaders(),
      });

      return response.ok;
    } catch (error) {
      return false;
    }
  }

  // Get user types
  async getUserTypes(): Promise<{ value: UserType; label: string }[]> {
    return Object.values(UserType).map(type => ({
      value: type,
      label: this.getUserTypeLabel(type),
    }));
  }

  // Get user type label
  private getUserTypeLabel(userType: UserType): string {
    const labels: Record<UserType, string> = {
      [UserType.ADMIN]: 'Administrator',
      [UserType.USER]: 'User',
      [UserType.GUEST]: 'Guest',
      [UserType.MANAGER]: 'Manager',
      [UserType.OPERATOR]: 'Operator',
    };
    return labels[userType] || userType;
  }

  // Get current user session
  getCurrentUserSession(): UserLoginResponse | null {
    if (typeof window === 'undefined') {
      return null;
    }

    try {
      const sessionData = localStorage.getItem('userSession');
      if (!sessionData) {
        return null;
      }

      const session = JSON.parse(sessionData) as UserLoginResponse;
      
      // Check if token is expired
      if (new Date(session.expiresAt) <= new Date()) {
        this.logout();
        return null;
      }

      return session;
    } catch (error) {
      console.error('Error parsing user session:', error);
      this.logout();
      return null;
    }
  }

  // Check if user is authenticated
  isAuthenticated(): boolean {
    return this.getCurrentUserSession() !== null;
  }

  // Get current user
  getCurrentUser(): UserResponse | null {
    const session = this.getCurrentUserSession();
    return session?.user || null;
  }

  // Refresh authentication token
  async refreshToken(): Promise<UserLoginResponse> {
    try {
      const response = await fetch(`${API_BASE_URL}/auth/refresh`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<UserLoginResponse>(response);
      
      // Update stored session
      if (typeof window !== 'undefined' && result.token) {
        localStorage.setItem('authToken', result.token);
        localStorage.setItem('userSession', JSON.stringify(result));
      }

      return result;
    } catch (error) {
      // If refresh fails, logout user
      this.logout();
      throw new Error(error instanceof Error ? error.message : 'Token refresh failed');
    }
  }

  // Validate user permissions
  async validateUserPermissions(userId: string, requiredPermissions: string[]): Promise<boolean> {
    try {
      const response = await fetch(`${API_BASE_URL}/users/${userId.toUpperCase()}/permissions`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify({ permissions: requiredPermissions }),
      });

      const result = await this.handleResponse<{ hasPermissions: boolean }>(response);
      return result.hasPermissions;
    } catch (error) {
      console.error('Permission validation failed:', error);
      return false;
    }
  }
}

// Export singleton instance
export const userService = new UserService();

// Export class for testing or custom instances
export { UserService };