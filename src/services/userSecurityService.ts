// src/services/userSecurityService.ts

import {
  SignonRequestDTO,
  SignonResponseDTO,
  UserSecurityDTO,
  CreateUserSecurityRequest,
  UpdateUserSecurityRequest,
  ChangePasswordRequest,
  ValidationResponseDTO,
  ApiResponse,
  ApiError,
  PaginatedResponse,
  UserListParams,
  UserSecurityFilters,
  UserSecuritySummary,
  BulkUserOperation,
  BulkOperationResult,
  UserSecurityHistory,
  PasswordPolicy,
  UserSecurityValidation,
  UserSecurityExport,
  UserSecurityImport,
  ImportResult,
  UserSecurityConfig
} from '@/types/userSecurity';

class UserSecurityService {
  private baseUrl: string;
  private authToken: string | null = null;

  constructor() {
    this.baseUrl = process.env.NEXT_PUBLIC_API_BASE_URL || '/api';
  }

  private setAuthToken(token: string): void {
    this.authToken = token;
  }

  private getAuthHeaders(): HeadersInit {
    const headers: HeadersInit = {
      'Content-Type': 'application/json',
    };

    if (this.authToken) {
      headers['Authorization'] = `Bearer ${this.authToken}`;
    }

    return headers;
  }

  private async handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
      const errorData: ApiError = await response.json().catch(() => ({
        message: 'An unexpected error occurred',
        code: 'UNKNOWN_ERROR',
        timestamp: new Date().toISOString()
      }));

      throw new Error(errorData.message || `HTTP ${response.status}: ${response.statusText}`);
    }

    const data = await response.json();
    return data;
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

  // Authentication Methods
  async signon(request: SignonRequestDTO): Promise<SignonResponseDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/auth/signon`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(request),
      });

      const result = await this.handleResponse<ApiResponse<SignonResponseDTO>>(response);
      
      if (result.data.success && result.data.user) {
        // Store auth token if provided in response headers
        const authHeader = response.headers.get('Authorization');
        if (authHeader) {
          this.setAuthToken(authHeader.replace('Bearer ', ''));
        }
      }

      return result.data;
    } catch (error) {
      throw new Error(`Signon failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async exit(): Promise<ApiResponse<void>> {
    try {
      const response = await fetch(`${this.baseUrl}/auth/exit`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<ApiResponse<void>>(response);
      
      // Clear auth token on successful exit
      this.authToken = null;
      
      return result;
    } catch (error) {
      throw new Error(`Exit failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async invalidKey(): Promise<ApiResponse<void>> {
    try {
      const response = await fetch(`${this.baseUrl}/auth/invalid-key`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<ApiResponse<void>>(response);
    } catch (error) {
      throw new Error(`Invalid key operation failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async validate(): Promise<ValidationResponseDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/auth/validate`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<ApiResponse<ValidationResponseDTO>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Validation failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  // User Management Methods
  async getUsers(params?: UserListParams): Promise<PaginatedResponse<UserSecurityDTO>> {
    try {
      const queryString = params ? this.buildQueryString(params) : '';
      const url = `${this.baseUrl}/users${queryString ? `?${queryString}` : ''}`;

      const response = await fetch(url, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<PaginatedResponse<UserSecurityDTO>>(response);
    } catch (error) {
      throw new Error(`Failed to fetch users: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async getUserById(userId: string): Promise<UserSecurityDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<ApiResponse<UserSecurityDTO>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to fetch user: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async createUser(userData: CreateUserSecurityRequest): Promise<UserSecurityDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/users`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(userData),
      });

      const result = await this.handleResponse<ApiResponse<UserSecurityDTO>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to create user: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async updateUser(userId: string, userData: UpdateUserSecurityRequest): Promise<UserSecurityDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(userData),
      });

      const result = await this.handleResponse<ApiResponse<UserSecurityDTO>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to update user: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async deleteUser(userId: string): Promise<ApiResponse<void>> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<ApiResponse<void>>(response);
    } catch (error) {
      throw new Error(`Failed to delete user: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  // User Status Management Methods
  async activateUser(userId: string): Promise<UserSecurityDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}/activate`, {
        method: 'PATCH',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<ApiResponse<UserSecurityDTO>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to activate user: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async deactivateUser(userId: string): Promise<UserSecurityDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}/deactivate`, {
        method: 'PATCH',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<ApiResponse<UserSecurityDTO>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to deactivate user: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async changePassword(userId: string, passwordData: ChangePasswordRequest): Promise<ApiResponse<void>> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}/change-password`, {
        method: 'PATCH',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(passwordData),
      });

      return await this.handleResponse<ApiResponse<void>>(response);
    } catch (error) {
      throw new Error(`Failed to change password: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  // Extended Methods for Enhanced Functionality
  async searchUsers(query: string, filters?: UserSecurityFilters): Promise<PaginatedResponse<UserSecurityDTO>> {
    try {
      const params = {
        search: query,
        ...filters,
      };

      return await this.getUsers(params);
    } catch (error) {
      throw new Error(`Failed to search users: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async getUserSummary(): Promise<UserSecuritySummary> {
    try {
      const response = await fetch(`${this.baseUrl}/users/summary`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<ApiResponse<UserSecuritySummary>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to fetch user summary: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async bulkUserOperation(operation: BulkUserOperation): Promise<BulkOperationResult> {
    try {
      const response = await fetch(`${this.baseUrl}/users/bulk`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(operation),
      });

      const result = await this.handleResponse<ApiResponse<BulkOperationResult>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Bulk operation failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async getUserHistory(userId: string): Promise<UserSecurityHistory[]> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}/history`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<ApiResponse<UserSecurityHistory[]>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to fetch user history: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async validateUserData(userData: Partial<CreateUserSecurityRequest>): Promise<UserSecurityValidation> {
    try {
      const response = await fetch(`${this.baseUrl}/users/validate`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(userData),
      });

      const result = await this.handleResponse<ApiResponse<UserSecurityValidation>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Validation failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async getPasswordPolicy(): Promise<PasswordPolicy> {
    try {
      const response = await fetch(`${this.baseUrl}/users/password-policy`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<ApiResponse<PasswordPolicy>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to fetch password policy: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async exportUsers(exportConfig: UserSecurityExport): Promise<Blob> {
    try {
      const response = await fetch(`${this.baseUrl}/users/export`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(exportConfig),
      });

      if (!response.ok) {
        throw new Error(`Export failed: ${response.statusText}`);
      }

      return await response.blob();
    } catch (error) {
      throw new Error(`Failed to export users: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async importUsers(importConfig: UserSecurityImport): Promise<ImportResult> {
    try {
      const formData = new FormData();
      formData.append('file', importConfig.file);
      formData.append('format', importConfig.format);
      formData.append('options', JSON.stringify(importConfig.options));

      const response = await fetch(`${this.baseUrl}/users/import`, {
        method: 'POST',
        headers: {
          'Authorization': this.authToken ? `Bearer ${this.authToken}` : '',
        },
        body: formData,
      });

      const result = await this.handleResponse<ApiResponse<ImportResult>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to import users: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async resetPassword(userId: string): Promise<ApiResponse<void>> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}/reset-password`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<ApiResponse<void>>(response);
    } catch (error) {
      throw new Error(`Failed to reset password: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async getUserConfig(): Promise<UserSecurityConfig> {
    try {
      const response = await fetch(`${this.baseUrl}/users/config`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<ApiResponse<UserSecurityConfig>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to fetch user config: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  async updateUserConfig(config: Partial<UserSecurityConfig>): Promise<UserSecurityConfig> {
    try {
      const response = await fetch(`${this.baseUrl}/users/config`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(config),
      });

      const result = await this.handleResponse<ApiResponse<UserSecurityConfig>>(response);
      return result.data;
    } catch (error) {
      throw new Error(`Failed to update user config: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  // Utility Methods
  isAuthenticated(): boolean {
    return this.authToken !== null;
  }

  clearAuth(): void {
    this.authToken = null;
  }

  setToken(token: string): void {
    this.setAuthToken(token);
  }

  getToken(): string | null {
    return this.authToken;
  }
}

// Export singleton instance
export const userSecurityService = new UserSecurityService();
export default userSecurityService;