import {
  UserSecurity,
  SignonRequestDTO,
  SignonResponseDTO,
  ValidateAuthRequest,
  ValidateAuthResponse,
  ExitRequest,
  ExitResponse,
  InvalidKeyRequest,
  InvalidKeyResponse,
  CreateUserSecurityRequest,
  UpdateUserSecurityRequest,
  ChangePasswordRequest,
  ChangePasswordResponse,
  GetUsersRequest,
  GetUsersResponse,
  GetUserByIdResponse,
  ActivateUserRequest,
  ActivateUserResponse,
  DeactivateUserRequest,
  DeactivateUserResponse,
  ApiResponse,
  ServiceResponse,
  UserSecurityServiceResponse,
  COSGN00CRequest,
  COSGN00CResponse,
  COUSR00CRequest,
  COUSR01CRequest,
  COUSR02CRequest,
  COUSR03CRequest,
  COUSRResponse
} from '@/types/userSecurity';

class UserSecurityService {
  private baseUrl: string;
  private token: string | null = null;

  constructor() {
    this.baseUrl = process.env.NEXT_PUBLIC_API_BASE_URL || '/api';
    this.token = this.getStoredToken();
  }

  private getStoredToken(): string | null {
    if (typeof window !== 'undefined') {
      return localStorage.getItem('auth_token') || sessionStorage.getItem('auth_token');
    }
    return null;
  }

  private setStoredToken(token: string, rememberMe: boolean = false): void {
    if (typeof window !== 'undefined') {
      if (rememberMe) {
        localStorage.setItem('auth_token', token);
        sessionStorage.removeItem('auth_token');
      } else {
        sessionStorage.setItem('auth_token', token);
        localStorage.removeItem('auth_token');
      }
      this.token = token;
    }
  }

  private removeStoredToken(): void {
    if (typeof window !== 'undefined') {
      localStorage.removeItem('auth_token');
      sessionStorage.removeItem('auth_token');
      this.token = null;
    }
  }

  private getAuthHeaders(): Record<string, string> {
    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
    };

    if (this.token) {
      headers['Authorization'] = `Bearer ${this.token}`;
    }

    return headers;
  }

  private async handleResponse<T>(response: Response): Promise<ServiceResponse<T>> {
    try {
      const data = await response.json();

      if (!response.ok) {
        return {
          success: false,
          error: data.message || `HTTP ${response.status}: ${response.statusText}`,
          statusCode: response.status,
        };
      }

      return {
        success: true,
        data: data.data || data,
        statusCode: response.status,
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Failed to parse response',
        statusCode: response.status,
      };
    }
  }

  private async makeRequest<T>(
    url: string,
    options: RequestInit = {}
  ): Promise<ServiceResponse<T>> {
    try {
      console.log('Making request to:', `${this.baseUrl}${url}`, options);
      const response = await fetch(`${this.baseUrl}${url}`, {
        ...options,
        headers: {
          ...this.getAuthHeaders(),
          ...options.headers
        },
      });

      return this.handleResponse<T>(response);
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Network request failed',
        statusCode: 0,
      };
    }
  }

  // COSGN00C - Authentication Business Logic
  private async executeCOSGN00C(request: COSGN00CRequest): Promise<ServiceResponse<COSGN00CResponse>> {
    const response = await this.makeRequest<COSGN00CResponse>('/users/signin', {
      method: 'POST',
      body: JSON.stringify(request),
    });

    return response;
  }

  // Authentication Methods
  async signon(credentials: SignonRequestDTO): UserSecurityServiceResponse<SignonResponseDTO> {
    try {
      const cosgn00cRequest: COSGN00CRequest = {
        functionKey: 'F1',
        userId: credentials.username,
        password: credentials.password,
        action: 'SIGNON',
      };

      const cosgn00cResponse = await this.executeCOSGN00C(cosgn00cRequest);

      if (!cosgn00cResponse.success) {
        return {
          success: false,
          error: cosgn00cResponse.error,
        };
      }

      const response = await this.makeRequest<SignonResponseDTO>('/users/signin', {
        method: 'POST',
        body: JSON.stringify(credentials),
      });

      if (response.success && response.data?.token) {
        this.setStoredToken(response.data.token, credentials.rememberMe || false);
      }

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Signon failed',
      };
    }
  }

  async exit(request: ExitRequest = {}): UserSecurityServiceResponse<ExitResponse> {
    try {
      const cosgn00cRequest: COSGN00CRequest = {
        functionKey: 'F3',
        action: 'EXIT',
      };

      await this.executeCOSGN00C(cosgn00cRequest);

      const response = await this.makeRequest<ExitResponse>('/auth/exit', {
        method: 'POST',
        body: JSON.stringify(request),
      });

      if (response.success) {
        this.removeStoredToken();
      }

      return response;
    } catch (error) {
      this.removeStoredToken();
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Exit failed',
      };
    }
  }

  async handleInvalidKey(request: InvalidKeyRequest): UserSecurityServiceResponse<InvalidKeyResponse> {
    try {
      const cosgn00cRequest: COSGN00CRequest = {
        functionKey: 'INVALID',
        userId: request.username,
        action: 'INVALID_KEY',
      };

      await this.executeCOSGN00C(cosgn00cRequest);

      const response = await this.makeRequest<InvalidKeyResponse>('/auth/invalid-key', {
        method: 'POST',
        body: JSON.stringify(request),
      });

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Invalid key handling failed',
      };
    }
  }

  async validateAuth(request: ValidateAuthRequest): UserSecurityServiceResponse<ValidateAuthResponse> {
    try {
      const cosgn00cRequest: COSGN00CRequest = {
        functionKey: 'VALIDATE',
        action: 'VALIDATE',
      };

      await this.executeCOSGN00C(cosgn00cRequest);

      const response = await this.makeRequest<ValidateAuthResponse>('/auth/validate', {
        method: 'POST',
        body: JSON.stringify(request),
      });

      if (!response.success || !response.data?.valid) {
        this.removeStoredToken();
      }

      return response;
    } catch (error) {
      this.removeStoredToken();
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Auth validation failed',
      };
    }
  }

  // COUSR00C - Get Users Business Logic
  private async executeCOUSR00C(request: COUSR00CRequest): Promise<ServiceResponse<COUSRResponse>> {
    const response = await this.makeRequest<COUSRResponse>('/users/cousr00c', {
      method: 'POST',
      body: JSON.stringify(request),
    });

    return response;
  }

  // COUSR01C - Create User Business Logic
  private async executeCOUSR01C(request: COUSR01CRequest): Promise<ServiceResponse<COUSRResponse>> {
    const response = await this.makeRequest<COUSRResponse>('/users/cousr01c', {
      method: 'POST',
      body: JSON.stringify(request),
    });

    return response;
  }

  // COUSR02C - Update User Business Logic
  private async executeCOUSR02C(request: COUSR02CRequest): Promise<ServiceResponse<COUSRResponse>> {
    const response = await this.makeRequest<COUSRResponse>('/users/cousr02c', {
      method: 'POST',
      body: JSON.stringify(request),
    });

    return response;
  }

  // COUSR03C - Delete/Activate/Deactivate/Change Password Business Logic
  private async executeCOUSR03C(request: COUSR03CRequest): Promise<ServiceResponse<COUSRResponse>> {
    const response = await this.makeRequest<COUSRResponse>('/users/cousr03c', {
      method: 'POST',
      body: JSON.stringify(request),
    });

    return response;
  }

  // User Management Methods
  async getUsers(params: GetUsersRequest = {}): UserSecurityServiceResponse<GetUsersResponse> {
    try {
      const cousr00cRequest: COUSR00CRequest = {
        action: params.search ? 'SEARCH' : 'LIST',
        searchCriteria: {
          username: params.search,
          isActive: params.isActive,
          roles: params.roles,
        },
        pagination: {
          page: params.page || 1,
          limit: params.limit || 10,
        },
      };

      await this.executeCOUSR00C(cousr00cRequest);

      const queryParams = new URLSearchParams();
      
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined && value !== null) {
          if (Array.isArray(value)) {
            value.forEach(item => queryParams.append(key, item.toString()));
          } else {
            queryParams.append(key, value.toString());
          }
        }
      });

      const response = await this.makeRequest<GetUsersResponse>(
        `/users?${queryParams.toString()}`,
        {
          method: 'GET',
        }
      );

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Failed to fetch users',
      };
    }
  }

  async getUserById(userId: string): UserSecurityServiceResponse<GetUserByIdResponse> {
    try {
      const response = await this.makeRequest<GetUserByIdResponse>(`/users/${userId}`, {
        method: 'GET',
      });

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Failed to fetch user',
      };
    }
  }

  async createUser(userData: CreateUserSecurityRequest): UserSecurityServiceResponse<UserSecurity> {
    try {
      const cousr01cRequest: COUSR01CRequest = {
        action: 'CREATE',
        userData,
      };

      await this.executeCOUSR01C(cousr01cRequest);

      const response = await this.makeRequest<UserSecurity>('/users', {
        method: 'POST',
        body: JSON.stringify(userData),
      });

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Failed to create user',
      };
    }
  }

  async updateUser(
    userId: string,
    userData: UpdateUserSecurityRequest
  ): UserSecurityServiceResponse<UserSecurity> {
    try {
      const cousr02cRequest: COUSR02CRequest = {
        action: 'UPDATE',
        userId,
        userData,
      };

      await this.executeCOUSR02C(cousr02cRequest);

      const response = await this.makeRequest<UserSecurity>(`/users/${userId}`, {
        method: 'PUT',
        body: JSON.stringify(userData),
      });

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Failed to update user',
      };
    }
  }

  async deleteUser(userId: string): UserSecurityServiceResponse<void> {
    try {
      const cousr03cRequest: COUSR03CRequest = {
        action: 'DELETE',
        userId,
      };

      await this.executeCOUSR03C(cousr03cRequest);

      const response = await this.makeRequest<void>(`/users/${userId}`, {
        method: 'DELETE',
      });

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Failed to delete user',
      };
    }
  }

  async activateUser(
    userId: string,
    request: ActivateUserRequest = {}
  ): UserSecurityServiceResponse<ActivateUserResponse> {
    try {
      const cousr03cRequest: COUSR03CRequest = {
        action: 'ACTIVATE',
        userId,
        reason: request.reason,
      };

      await this.executeCOUSR03C(cousr03cRequest);

      const response = await this.makeRequest<ActivateUserResponse>(`/users/${userId}/activate`, {
        method: 'PATCH',
        body: JSON.stringify(request),
      });

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Failed to activate user',
      };
    }
  }

  async deactivateUser(
    userId: string,
    request: DeactivateUserRequest = {}
  ): UserSecurityServiceResponse<DeactivateUserResponse> {
    try {
      const cousr03cRequest: COUSR03CRequest = {
        action: 'DEACTIVATE',
        userId,
        reason: request.reason,
      };

      await this.executeCOUSR03C(cousr03cRequest);

      const response = await this.makeRequest<DeactivateUserResponse>(`/users/${userId}/deactivate`, {
        method: 'PATCH',
        body: JSON.stringify(request),
      });

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Failed to deactivate user',
      };
    }
  }

  async changePassword(
    userId: string,
    passwordData: ChangePasswordRequest
  ): UserSecurityServiceResponse<ChangePasswordResponse> {
    try {
      const cousr03cRequest: COUSR03CRequest = {
        action: 'CHANGE_PASSWORD',
        userId,
        passwordData,
      };

      await this.executeCOUSR03C(cousr03cRequest);

      const response = await this.makeRequest<ChangePasswordResponse>(
        `/users/${userId}/change-password`,
        {
          method: 'PATCH',
          body: JSON.stringify(passwordData),
        }
      );

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Failed to change password',
      };
    }
  }

  // Utility Methods
  async refreshToken(): UserSecurityServiceResponse<{ token: string; refreshToken: string }> {
    try {
      const refreshToken = typeof window !== 'undefined' 
        ? localStorage.getItem('refresh_token') || sessionStorage.getItem('refresh_token')
        : null;

      if (!refreshToken) {
        return {
          success: false,
          error: 'No refresh token available',
        };
      }

      const response = await this.makeRequest<{ token: string; refreshToken: string }>(
        '/auth/refresh',
        {
          method: 'POST',
          body: JSON.stringify({ refreshToken }),
        }
      );

      if (response.success && response.data?.token) {
        this.setStoredToken(response.data.token, !!localStorage.getItem('auth_token'));
        
        if (response.data.refreshToken) {
          if (typeof window !== 'undefined') {
            if (localStorage.getItem('refresh_token')) {
              localStorage.setItem('refresh_token', response.data.refreshToken);
            } else {
              sessionStorage.setItem('refresh_token', response.data.refreshToken);
            }
          }
        }
      }

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Token refresh failed',
      };
    }
  }

  getCurrentToken(): string | null {
    return this.token;
  }

  isAuthenticated(): boolean {
    return !!this.token;
  }

  clearAuth(): void {
    this.removeStoredToken();
    if (typeof window !== 'undefined') {
      localStorage.removeItem('refresh_token');
      sessionStorage.removeItem('refresh_token');
    }
  }

  // Health check method
  async healthCheck(): UserSecurityServiceResponse<{ status: string; timestamp: string }> {
    try {
      const response = await this.makeRequest<{ status: string; timestamp: string }>(
        '/health',
        {
          method: 'GET',
        }
      );

      return response;
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Health check failed',
      };
    }
  }
}

// Export singleton instance
const userSecurityService = new UserSecurityService();
export default userSecurityService;

// Export class for testing or multiple instances
export { UserSecurityService };