// src/services/userSecurityService.ts
import { 
  UserSecurity,
  UserSecurityDTO, 
  CreateUserSecurityRequest,
  UpdateUserSecurityRequest,
  UserSecurityQuery,
  UserSecurityFilters,
  ValidationResponseDTO,
  ApiResponse,
  PaginatedResponse,
  BulkUserOperation,
  BulkOperationResult,
  UserSecurityAuditLog,
  ValidationError,
  UserType,
  UserStatus
} from '@/types/userSecurity';

const API_BASE_URL = '/api';

interface AuthHeaders {
  'Content-Type': string;
  'Authorization'?: string;
}

class UserSecurityService {
  private getAuthHeaders(): AuthHeaders {
    const headers: AuthHeaders = {
      'Content-Type': 'application/json',
    };

    // Get token from localStorage if available (client-side only)
    if (typeof window !== 'undefined') {
      const token = localStorage.getItem('auth_token');
      if (token) {
        headers['Authorization'] = `Bearer ${token}`;
      }
    }

    return headers;
  }

  private async handleResponse<T>(response: Response): Promise<ApiResponse<T>> {
    const data = await response.json().catch(() => ({}));
    
    if (!response.ok) {
      const errorMessage = data.message || 
        data.detail?.[0]?.msg || 
        data.error ||
        `HTTP ${response.status}: ${response.statusText}`;
      
      throw new Error(errorMessage);
    }

    return {
      success: true,
      data: data.data || data,
      message: data.message,
      errors: data.errors,
      timestamp: new Date().toISOString()
    };
  }

  private buildQueryString(params: Record<string, any>): string {
    const searchParams = new URLSearchParams();
    
    Object.entries(params).forEach(([key, value]) => {
      if (value !== undefined && value !== null && value !== '') {
        if (Array.isArray(value)) {
          value.forEach(item => searchParams.append(key, item.toString()));
        } else {
          searchParams.append(key, value.toString());
        }
      }
    });

    return searchParams.toString();
  }

  private transformToDTO(userSecurity: UserSecurity): UserSecurityDTO {
    const { password, ...userDTO } = userSecurity;
    return {
      ...userDTO,
      fullName: `${userSecurity.firstName} ${userSecurity.lastName}`.trim()
    };
  }

  private validateUserSecurityData(data: CreateUserSecurityRequest | UpdateUserSecurityRequest): ValidationResponseDTO {
    const errors: ValidationError[] = [];

    // Validate firstName (SEC-USR-FNAME: 20 chars)
    if ('firstName' in data && data.firstName !== undefined) {
      if (!data.firstName || data.firstName.trim().length === 0) {
        errors.push({ field: 'firstName', message: 'First name is required' });
      } else if (data.firstName.trim().length > 20) {
        errors.push({ field: 'firstName', message: 'First name must not exceed 20 characters' });
      } else if (!/^[A-Za-z\s'-]+$/.test(data.firstName.trim())) {
        errors.push({ field: 'firstName', message: 'First name must contain only letters, spaces, hyphens, and apostrophes' });
      }
    }

    // Validate lastName (SEC-USR-LNAME: 20 chars)
    if ('lastName' in data && data.lastName !== undefined) {
      if (!data.lastName || data.lastName.trim().length === 0) {
        errors.push({ field: 'lastName', message: 'Last name is required' });
      } else if (data.lastName.trim().length > 20) {
        errors.push({ field: 'lastName', message: 'Last name must not exceed 20 characters' });
      } else if (!/^[A-Za-z\s'-]+$/.test(data.lastName.trim())) {
        errors.push({ field: 'lastName', message: 'Last name must contain only letters, spaces, hyphens, and apostrophes' });
      }
    }

    // Validate password (SEC-USR-PWD: 8 chars) - only for create operations
    if ('password' in data && data.password !== undefined) {
      if (!data.password || data.password.length === 0) {
        errors.push({ field: 'password', message: 'Password is required' });
      } else if (data.password.length < 6) {
        errors.push({ field: 'password', message: 'Password must be at least 6 characters long' });
      } else if (data.password.length > 20) {
        errors.push({ field: 'password', message: 'Password must not exceed 20 characters' });
      }
    }

    // Validate userType (SEC-USR-TYPE: 1 char)
    if ('userType' in data && data.userType !== undefined) {
      if (!data.userType || data.userType.trim().length === 0) {
        errors.push({ field: 'userType', message: 'User type is required' });
      } else if (!Object.values(UserType).includes(data.userType as UserType)) {
        errors.push({ 
          field: 'userType', 
          message: `User type must be one of: ${Object.values(UserType).join(', ')}` 
        });
      }
    }

    return {
      isValid: errors.length === 0,
      errors
    };
  }

  // CRUD Operations
  async getAllUsers(query?: UserSecurityQuery): Promise<PaginatedResponse<UserSecurityDTO>> {
    try {
      const queryString = query ? this.buildQueryString(query) : '';
      const url = `${API_BASE_URL}/users${queryString ? `?${queryString}` : ''}`;
      
      const response = await fetch(url, {
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<PaginatedResponse<UserSecurityDTO>>(response);
      const data = result.data as PaginatedResponse<UserSecurityDTO>;

      // Transform users to DTOs if needed
      if (data.data) {
        data.data = data.data.map(user => ({
          ...user,
          fullName: `${user.firstName} ${user.lastName}`.trim()
        }));
      }

      return data;
    } catch (error) {
      console.error('Get all users error:', error);
      throw error;
    }
  }

  async getUserById(id: string): Promise<UserSecurityDTO> {
    try {
      if (!id || id.trim().length === 0) {
        throw new Error('User ID is required');
      }

      if (id.length > 8) {
        throw new Error('User ID must not exceed 8 characters');
      }

      const response = await fetch(`${API_BASE_URL}/users/${id.toUpperCase()}`, {
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<UserSecurityDTO>(response);
      const user = result.data as UserSecurityDTO;

      return {
        ...user,
        fullName: `${user.firstName} ${user.lastName}`.trim()
      };
    } catch (error) {
      console.error('Get user by ID error:', error);
      throw error;
    }
  }

  async createUser(userData: CreateUserSecurityRequest): Promise<UserSecurityDTO> {
    try {
      // Validate input data
      const validation = this.validateUserSecurityData(userData);
      if (!validation.isValid) {
        const error = new Error('Validation failed');
        (error as any).validationErrors = validation.errors;
        throw error;
      }

      const response = await fetch(`${API_BASE_URL}/users`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify({
          ...userData,
          firstName: userData.firstName.trim(),
          lastName: userData.lastName.trim(),
          userType: userData.userType.toUpperCase(),
          isActive: userData.isActive !== undefined ? userData.isActive : true
        }),
      });

      const result = await this.handleResponse<UserSecurityDTO>(response);
      const user = result.data as UserSecurityDTO;

      return {
        ...user,
        fullName: `${user.firstName} ${user.lastName}`.trim()
      };
    } catch (error) {
      console.error('Create user error:', error);
      throw error;
    }
  }

  async updateUser(id: string, userData: UpdateUserSecurityRequest): Promise<UserSecurityDTO> {
    try {
      if (!id || id.trim().length === 0) {
        throw new Error('User ID is required');
      }

      // Validate input data
      const validation = this.validateUserSecurityData(userData);
      if (!validation.isValid) {
        const error = new Error('Validation failed');
        (error as any).validationErrors = validation.errors;
        throw error;
      }

      const updateData: any = {};
      
      if (userData.firstName !== undefined) {
        updateData.firstName = userData.firstName.trim();
      }
      if (userData.lastName !== undefined) {
        updateData.lastName = userData.lastName.trim();
      }
      if (userData.userType !== undefined) {
        updateData.userType = userData.userType.toUpperCase();
      }
      if (userData.isActive !== undefined) {
        updateData.isActive = userData.isActive;
      }

      const response = await fetch(`${API_BASE_URL}/users/${id.toUpperCase()}`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(updateData),
      });

      const result = await this.handleResponse<UserSecurityDTO>(response);
      const user = result.data as UserSecurityDTO;

      return {
        ...user,
        fullName: `${user.firstName} ${user.lastName}`.trim()
      };
    } catch (error) {
      console.error('Update user error:', error);
      throw error;
    }
  }

  async deleteUser(id: string): Promise<void> {
    try {
      if (!id || id.trim().length === 0) {
        throw new Error('User ID is required');
      }

      const response = await fetch(`${API_BASE_URL}/users/${id.toUpperCase()}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });

      await this.handleResponse<void>(response);
    } catch (error) {
      console.error('Delete user error:', error);
      throw error;
    }
  }

  // Search and Filter Operations
  async searchUsers(searchTerm: string, filters?: UserSecurityFilters): Promise<UserSecurityDTO[]> {
    try {
      const query: UserSecurityQuery = {
        search: searchTerm,
        ...filters,
        pageSize: 100 // Get more results for search
      };

      const result = await this.getAllUsers(query);
      return result.data;
    } catch (error) {
      console.error('Search users error:', error);
      throw error;
    }
  }

  async getUsersByType(userType: UserType): Promise<UserSecurityDTO[]> {
    try {
      const query: UserSecurityQuery = {
        userType,
        pageSize: 100
      };

      const result = await this.getAllUsers(query);
      return result.data;
    } catch (error) {
      console.error('Get users by type error:', error);
      throw error;
    }
  }

  async getActiveUsers(): Promise<UserSecurityDTO[]> {
    try {
      const query: UserSecurityQuery = {
        isActive: true,
        pageSize: 100
      };

      const result = await this.getAllUsers(query);
      return result.data;
    } catch (error) {
      console.error('Get active users error:', error);
      throw error;
    }
  }

  async getInactiveUsers(): Promise<UserSecurityDTO[]> {
    try {
      const query: UserSecurityQuery = {
        isActive: false,
        pageSize: 100
      };

      const result = await this.getAllUsers(query);
      return result.data;
    } catch (error) {
      console.error('Get inactive users error:', error);
      throw error;
    }
  }

  // Bulk Operations
  async bulkUpdateUsers(operation: BulkUserOperation): Promise<BulkOperationResult> {
    try {
      if (!operation.userIds || operation.userIds.length === 0) {
        throw new Error('At least one user ID is required');
      }

      const response = await fetch(`${API_BASE_URL}/users/bulk`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(operation),
      });

      const result = await this.handleResponse<BulkOperationResult>(response);
      return result.data as BulkOperationResult;
    } catch (error) {
      console.error('Bulk update users error:', error);
      throw error;
    }
  }

  async activateUsers(userIds: string[]): Promise<BulkOperationResult> {
    return this.bulkUpdateUsers({
      userIds,
      operation: 'activate'
    });
  }

  async deactivateUsers(userIds: string[]): Promise<BulkOperationResult> {
    return this.bulkUpdateUsers({
      userIds,
      operation: 'deactivate'
    });
  }

  async deleteUsers(userIds: string[]): Promise<BulkOperationResult> {
    return this.bulkUpdateUsers({
      userIds,
      operation: 'delete'
    });
  }

  async changeUserType(userIds: string[], newUserType: UserType): Promise<BulkOperationResult> {
    return this.bulkUpdateUsers({
      userIds,
      operation: 'changeType',
      newUserType
    });
  }

  // Audit and Logging
  async getUserAuditLog(userId: string, limit: number = 50): Promise<UserSecurityAuditLog[]> {
    try {
      if (!userId || userId.trim().length === 0) {
        throw new Error('User ID is required');
      }

      const response = await fetch(`${API_BASE_URL}/users/${userId.toUpperCase()}/audit?limit=${limit}`, {
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<UserSecurityAuditLog[]>(response);
      return result.data as UserSecurityAuditLog[];
    } catch (error) {
      console.error('Get user audit log error:', error);
      throw error;
    }
  }

  async getAllAuditLogs(limit: number = 100): Promise<UserSecurityAuditLog[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/users/audit?limit=${limit}`, {
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<UserSecurityAuditLog[]>(response);
      return result.data as UserSecurityAuditLog[];
    } catch (error) {
      console.error('Get all audit logs error:', error);
      throw error;
    }
  }

  // Validation and Utility Methods
  async validateUserId(userId: string): Promise<ValidationResponseDTO> {
    const errors: ValidationError[] = [];

    if (!userId || userId.trim().length === 0) {
      errors.push({ field: 'userId', message: 'User ID is required' });
    } else {
      const trimmedId = userId.trim();
      
      if (trimmedId.length > 8) {
        errors.push({ field: 'userId', message: 'User ID must not exceed 8 characters' });
      }
      
      if (!/^[A-Za-z0-9]+$/.test(trimmedId)) {
        errors.push({ field: 'userId', message: 'User ID must contain only letters and numbers' });
      }

      // Check if user ID already exists (only if validation passes so far)
      if (errors.length === 0) {
        try {
          await this.getUserById(trimmedId);
          errors.push({ field: 'userId', message: 'User ID already exists' });
        } catch (error) {
          // User doesn't exist, which is good for validation
        }
      }
    }

    return {
      isValid: errors.length === 0,
      errors
    };
  }

  async checkUserExists(userId: string): Promise<boolean> {
    try {
      await this.getUserById(userId);
      return true;
    } catch (error) {
      return false;
    }
  }

  // Statistics and Reporting
  async getUserStatistics(): Promise<{
    totalUsers: number;
    activeUsers: number;
    inactiveUsers: number;
    usersByType: Record<string, number>;
    recentLogins: number;
  }> {
    try {
      const response = await fetch(`${API_BASE_URL}/users/statistics`, {
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<any>(response);
      return result.data;
    } catch (error) {
      console.error('Get user statistics error:', error);
      throw error;
    }
  }

  // Export/Import Operations
  async exportUsers(format: 'csv' | 'json' = 'csv', filters?: UserSecurityFilters): Promise<Blob> {
    try {
      const queryParams = {
        format,
        ...filters
      };

      const queryString = this.buildQueryString(queryParams);
      const response = await fetch(`${API_BASE_URL}/users/export?${queryString}`, {
        headers: this.getAuthHeaders(),
      });

      if (!response.ok) {
        throw new Error(`Export failed: ${response.statusText}`);
      }

      return await response.blob();
    } catch (error) {
      console.error('Export users error:', error);
      throw error;
    }
  }

  async importUsers(file: File): Promise<{
    success: boolean;
    importedCount: number;
    failedCount: number;
    errors: ValidationError[];
  }> {
    try {
      const formData = new FormData();
      formData.append('file', file);

      const response = await fetch(`${API_BASE_URL}/users/import`, {
        method: 'POST',
        headers: {
          'Authorization': this.getAuthHeaders()['Authorization'] || '',
        },
        body: formData,
      });

      const result = await this.handleResponse<any>(response);
      return result.data;
    } catch (error) {
      console.error('Import users error:', error);
      throw error;
    }
  }

  // Password Management
  async resetUserPassword(userId: string): Promise<{ temporaryPassword: string }> {
    try {
      if (!userId || userId.trim().length === 0) {
        throw new Error('User ID is required');
      }

      const response = await fetch(`${API_BASE_URL}/users/${userId.toUpperCase()}/reset-password`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<{ temporaryPassword: string }>(response);
      return result.data as { temporaryPassword: string };
    } catch (error) {
      console.error('Reset user password error:', error);
      throw error;
    }
  }

  async forcePasswordChange(userId: string): Promise<void> {
    try {
      if (!userId || userId.trim().length === 0) {
        throw new Error('User ID is required');
      }

      const response = await fetch(`${API_BASE_URL}/users/${userId.toUpperCase()}/force-password-change`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });

      await this.handleResponse<void>(response);
    } catch (error) {
      console.error('Force password change error:', error);
      throw error;
    }
  }
}

export const userSecurityService = new UserSecurityService();
export default userSecurityService;