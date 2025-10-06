// src/api/userApi.ts

import {
  User,
  CreateUserRequest,
  UpdateUserRequest,
  SignonRequest,
  SignonResponse,
  AuthUser,
  PaginatedUsersResponse,
  APIResponse,
  UserSearchParams,
  UserStats,
  PasswordChangeRequest,
  RefreshTokenRequest,
  RefreshTokenResponse,
} from '@/types/user';

export interface ApiConfig {
  baseURL: string;
  timeout: number;
  headers: Record<string, string>;
}

export interface ApiClient {
  get<T>(url: string, config?: RequestInit): Promise<APIResponse<T>>;
  post<T>(url: string, data?: any, config?: RequestInit): Promise<APIResponse<T>>;
  put<T>(url: string, data?: any, config?: RequestInit): Promise<APIResponse<T>>;
  patch<T>(url: string, data?: any, config?: RequestInit): Promise<APIResponse<T>>;
  delete<T>(url: string, config?: RequestInit): Promise<APIResponse<T>>;
}

class UserApiClient implements ApiClient {
  private config: ApiConfig;

  constructor(config: Partial<ApiConfig> = {}) {
    this.config = {
      baseURL: '/api',
      timeout: 30000,
      headers: {
        'Content-Type': 'application/json',
      },
      ...config,
    };
  }

  private async handleResponse<T>(response: Response): Promise<APIResponse<T>> {
    let data: any;
    
    try {
      const text = await response.text();
      data = text ? JSON.parse(text) : {};
    } catch (error) {
      data = {};
    }

    if (!response.ok) {
      return {
        success: false,
        error: {
          error: data.error || `HTTP ${response.status}`,
          message: data.message || response.statusText,
          details: data.details,
          statusCode: response.status,
        },
        message: data.message || response.statusText,
      };
    }

    return {
      success: true,
      data: data,
      message: data.message,
    };
  }

  private async makeRequest<T>(
    url: string,
    options: RequestInit = {}
  ): Promise<APIResponse<T>> {
    try {
      const fullUrl = url.startsWith('http') ? url : `${this.config.baseURL}${url}`;
      
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), this.config.timeout);

      const response = await fetch(fullUrl, {
        ...options,
        headers: {
          ...this.config.headers,
          ...options.headers,
        },
        signal: controller.signal,
      });

      clearTimeout(timeoutId);
      return await this.handleResponse<T>(response);
    } catch (error) {
      if (error instanceof Error && error.name === 'AbortError') {
        return {
          success: false,
          error: {
            error: 'Timeout Error',
            message: 'Request timed out',
            statusCode: 408,
          },
        };
      }

      return {
        success: false,
        error: {
          error: 'Network Error',
          message: error instanceof Error ? error.message : 'Unknown error',
          statusCode: 0,
        },
      };
    }
  }

  public async get<T>(url: string, config?: RequestInit): Promise<APIResponse<T>> {
    return this.makeRequest<T>(url, {
      method: 'GET',
      ...config,
    });
  }

  public async post<T>(url: string, data?: any, config?: RequestInit): Promise<APIResponse<T>> {
    return this.makeRequest<T>(url, {
      method: 'POST',
      body: data ? JSON.stringify(data) : undefined,
      ...config,
    });
  }

  public async put<T>(url: string, data?: any, config?: RequestInit): Promise<APIResponse<T>> {
    return this.makeRequest<T>(url, {
      method: 'PUT',
      body: data ? JSON.stringify(data) : undefined,
      ...config,
    });
  }

  public async patch<T>(url: string, data?: any, config?: RequestInit): Promise<APIResponse<T>> {
    return this.makeRequest<T>(url, {
      method: 'PATCH',
      body: data ? JSON.stringify(data) : undefined,
      ...config,
    });
  }

  public async delete<T>(url: string, config?: RequestInit): Promise<APIResponse<T>> {
    return this.makeRequest<T>(url, {
      method: 'DELETE',
      ...config,
    });
  }

  public setAuthToken(token: string): void {
    this.config.headers['Authorization'] = `Bearer ${token}`;
  }

  public removeAuthToken(): void {
    delete this.config.headers['Authorization'];
  }

  public updateConfig(newConfig: Partial<ApiConfig>): void {
    this.config = { ...this.config, ...newConfig };
  }
}

// API Endpoints
export class UserApi {
  private client: ApiClient;

  constructor(client?: ApiClient) {
    this.client = client || new UserApiClient();
  }

  // Authentication Endpoints
  public async signon(request: SignonRequest): Promise<APIResponse<SignonResponse>> {
    return this.client.post<SignonResponse>('/auth/signon', request);
  }

  public async exit(): Promise<APIResponse<void>> {
    return this.client.post<void>('/auth/exit');
  }

  public async validateAuth(): Promise<APIResponse<AuthUser>> {
    return this.client.post<AuthUser>('/auth/validate');
  }

  public async invalidKey(): Promise<APIResponse<void>> {
    return this.client.post<void>('/auth/invalid-key');
  }

  public async refreshToken(request: RefreshTokenRequest): Promise<APIResponse<RefreshTokenResponse>> {
    return this.client.post<RefreshTokenResponse>('/auth/refresh', request);
  }

  // User Management Endpoints
  public async getUsers(params?: UserSearchParams): Promise<APIResponse<PaginatedUsersResponse>> {
    const queryParams = new URLSearchParams();
    
    if (params) {
      if (params.page) queryParams.append('page', params.page.toString());
      if (params.limit) queryParams.append('size', params.limit.toString());
      if (params.sortBy) queryParams.append('sort', params.sortBy);
      if (params.filters?.userType) queryParams.append('userType', params.filters.userType);
      if (params.filters?.search) queryParams.append('search', params.filters.search);
    }

    const url = `/users${queryParams.toString() ? `?${queryParams.toString()}` : ''}`;
    return this.client.get<PaginatedUsersResponse>(url);
  }

  public async getUserById(userId: string): Promise<APIResponse<User>> {
    return this.client.get<User>(`/users/${userId}`);
  }

  public async createUser(userData: CreateUserRequest): Promise<APIResponse<User>> {
    return this.client.post<User>('/users', userData);
  }

  public async updateUser(userId: string, userData: UpdateUserRequest): Promise<APIResponse<User>> {
    return this.client.put<User>(`/users/${userId}`, userData);
  }

  public async deleteUser(userId: string): Promise<APIResponse<void>> {
    return this.client.delete<void>(`/users/${userId}`);
  }

  public async activateUser(userId: string): Promise<APIResponse<User>> {
    return this.client.patch<User>(`/users/${userId}/activate`);
  }

  public async deactivateUser(userId: string): Promise<APIResponse<User>> {
    return this.client.patch<User>(`/users/${userId}/deactivate`);
  }

  public async changePassword(userId: string, passwordData: PasswordChangeRequest): Promise<APIResponse<void>> {
    return this.client.patch<void>(`/users/${userId}/change-password`, {
      newPassword: passwordData.newPassword,
    });
  }

  public async getUserStats(): Promise<APIResponse<UserStats>> {
    return this.client.get<UserStats>('/users/stats');
  }

  // Utility methods
  public setAuthToken(token: string): void {
    if (this.client instanceof UserApiClient) {
      this.client.setAuthToken(token);
    }
  }

  public removeAuthToken(): void {
    if (this.client instanceof UserApiClient) {
      this.client.removeAuthToken();
    }
  }
}

// Export singleton instance
export const userApi = new UserApi();

// Export types for external use
export type {
  ApiConfig,
  ApiClient,
};

// Export the client class for custom implementations
export { UserApiClient };