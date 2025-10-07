// src/services/userSecurityService.ts
import {
  SignonRequestDTO,
  SignonResponseDTO,
  ExitRequestDTO,
  ExitResponseDTO,
  InvalidKeyRequestDTO,
  InvalidKeyResponseDTO,
  ValidationResponseDTO,
  UserSecurityDTO,
  CreateUserSecurityRequest,
  UpdateUserSecurityRequest,
  ChangePasswordRequest,
  UserSecurityListResponse,
  UserSecuritySearchCriteria,
  PasswordPolicy,
  SecurityAuditLog,
  APIError,
  HTTPValidationError
} from '@/types/user-security';

const API_BASE_URL = '/api';

class UserSecurityService {
  private async handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
      let errorData: APIError | HTTPValidationError;
      
      try {
        errorData = await response.json();
      } catch {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      if ('detail' in errorData) {
        const validationError = errorData as HTTPValidationError;
        const errorMessage = validationError.detail
          .map(err => `${err.loc.join('.')}: ${err.msg}`)
          .join(', ');
        throw new Error(errorMessage);
      } else {
        const apiError = errorData as APIError;
        throw new Error(apiError.message || `HTTP ${response.status}: ${response.statusText}`);
      }
    }

    const contentType = response.headers.get('content-type');
    if (contentType && contentType.includes('application/json')) {
      return response.json();
    }
    
    return response.text() as unknown as T;
  }

  private getAuthHeaders(): Record<string, string> {
    const token = typeof window !== 'undefined' ? localStorage.getItem('access_token') : null;
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  // Authentication Methods
  async signon(signonRequest: SignonRequestDTO): Promise<SignonResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/auth/signon`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(signonRequest),
    });

    const result = await this.handleResponse<SignonResponseDTO>(response);
    
    // Store session information if successful
    if (result.success && typeof window !== 'undefined') {
      localStorage.setItem('access_token', result.sessionToken);
      localStorage.setItem('session_id', result.sessionId);
      localStorage.setItem('user_id', result.userId);
      localStorage.setItem('user_security', JSON.stringify(result.userSecurity));
    }

    return result;
  }

  async exit(exitRequest: ExitRequestDTO): Promise<ExitResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/auth/exit`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(exitRequest),
    });

    const result = await this.handleResponse<ExitResponseDTO>(response);
    
    // Clear session information if successful
    if (result.success && typeof window !== 'undefined') {
      this.clearSession();
    }

    return result;
  }

  async handleInvalidKey(invalidKeyRequest: InvalidKeyRequestDTO): Promise<InvalidKeyResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/auth/invalid-key`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(invalidKeyRequest),
    });

    return this.handleResponse<InvalidKeyResponseDTO>(response);
  }

  async validateCredentials(userId: string, password: string): Promise<ValidationResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/auth/validate`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ userId, password }),
    });

    return this.handleResponse<ValidationResponseDTO>(response);
  }

  // User Security Management Methods
  async getUserSecurity(userId: string): Promise<UserSecurityDTO> {
    const response = await fetch(`${API_BASE_URL}/user-security/${encodeURIComponent(userId)}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    return this.handleResponse<UserSecurityDTO>(response);
  }

  async listUserSecurity(searchCriteria?: UserSecuritySearchCriteria): Promise<UserSecurityListResponse> {
    const queryParams = new URLSearchParams();
    
    if (searchCriteria) {
      Object.entries(searchCriteria).forEach(([key, value]) => {
        if (value !== undefined && value !== null && value !== '') {
          queryParams.append(key, String(value));
        }
      });
    }

    const url = `${API_BASE_URL}/user-security${queryParams.toString() ? `?${queryParams.toString()}` : ''}`;
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    return this.handleResponse<UserSecurityListResponse>(response);
  }

  async createUserSecurity(createRequest: CreateUserSecurityRequest): Promise<UserSecurityDTO> {
    const response = await fetch(`${API_BASE_URL}/user-security`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(createRequest),
    });

    return this.handleResponse<UserSecurityDTO>(response);
  }

  async updateUserSecurity(userId: string, updateRequest: UpdateUserSecurityRequest): Promise<UserSecurityDTO> {
    const response = await fetch(`${API_BASE_URL}/user-security/${encodeURIComponent(userId)}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(updateRequest),
    });

    return this.handleResponse<UserSecurityDTO>(response);
  }

  async deleteUserSecurity(userId: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/user-security/${encodeURIComponent(userId)}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });

    await this.handleResponse<void>(response);
  }

  async changePassword(changePasswordRequest: ChangePasswordRequest): Promise<ValidationResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/user-security/change-password`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(changePasswordRequest),
    });

    return this.handleResponse<ValidationResponseDTO>(response);
  }

  async resetPassword(userId: string, newPassword: string, confirmPassword: string): Promise<ValidationResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/user-security/reset-password`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify({
        userId,
        newPassword,
        confirmPassword,
      }),
    });

    return this.handleResponse<ValidationResponseDTO>(response);
  }

  async unlockAccount(userId: string): Promise<ValidationResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/user-security/${encodeURIComponent(userId)}/unlock`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });

    return this.handleResponse<ValidationResponseDTO>(response);
  }

  async lockAccount(userId: string, reason?: string): Promise<ValidationResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/user-security/${encodeURIComponent(userId)}/lock`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify({ reason }),
    });

    return this.handleResponse<ValidationResponseDTO>(response);
  }

  async activateAccount(userId: string): Promise<ValidationResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/user-security/${encodeURIComponent(userId)}/activate`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });

    return this.handleResponse<ValidationResponseDTO>(response);
  }

  async deactivateAccount(userId: string, reason?: string): Promise<ValidationResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/user-security/${encodeURIComponent(userId)}/deactivate`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify({ reason }),
    });

    return this.handleResponse<ValidationResponseDTO>(response);
  }

  // Password Policy Methods
  async getPasswordPolicy(): Promise<PasswordPolicy> {
    const response = await fetch(`${API_BASE_URL}/user-security/password-policy`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    return this.handleResponse<PasswordPolicy>(response);
  }

  async updatePasswordPolicy(passwordPolicy: PasswordPolicy): Promise<PasswordPolicy> {
    const response = await fetch(`${API_BASE_URL}/user-security/password-policy`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(passwordPolicy),
    });

    return this.handleResponse<PasswordPolicy>(response);
  }

  async validatePassword(password: string, userId?: string): Promise<ValidationResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/user-security/validate-password`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify({ password, userId }),
    });

    return this.handleResponse<ValidationResponseDTO>(response);
  }

  // Audit Log Methods
  async getSecurityAuditLogs(
    userId?: string,
    action?: string,
    dateFrom?: string,
    dateTo?: string,
    page?: number,
    pageSize?: number
  ): Promise<{ logs: SecurityAuditLog[]; totalCount: number; currentPage: number; totalPages: number }> {
    const queryParams = new URLSearchParams();
    
    if (userId) queryParams.append('userId', userId);
    if (action) queryParams.append('action', action);
    if (dateFrom) queryParams.append('dateFrom', dateFrom);
    if (dateTo) queryParams.append('dateTo', dateTo);
    if (page) queryParams.append('page', String(page));
    if (pageSize) queryParams.append('pageSize', String(pageSize));

    const url = `${API_BASE_URL}/user-security/audit-logs${queryParams.toString() ? `?${queryParams.toString()}` : ''}`;
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    return this.handleResponse<{ logs: SecurityAuditLog[]; totalCount: number; currentPage: number; totalPages: number }>(response);
  }

  // Session Management Methods
  async getCurrentSession(): Promise<{ sessionId: string; userId: string; expiryTime: string } | null> {
    if (typeof window === 'undefined') return null;
    
    const sessionId = localStorage.getItem('session_id');
    const userId = localStorage.getItem('user_id');
    
    if (!sessionId || !userId) return null;

    try {
      const response = await fetch(`${API_BASE_URL}/auth/session`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      return this.handleResponse<{ sessionId: string; userId: string; expiryTime: string }>(response);
    } catch {
      this.clearSession();
      return null;
    }
  }

  async refreshSession(): Promise<SignonResponseDTO> {
    const response = await fetch(`${API_BASE_URL}/auth/refresh`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });

    const result = await this.handleResponse<SignonResponseDTO>(response);
    
    // Update session information if successful
    if (result.success && typeof window !== 'undefined') {
      localStorage.setItem('access_token', result.sessionToken);
      localStorage.setItem('session_id', result.sessionId);
      localStorage.setItem('user_security', JSON.stringify(result.userSecurity));
    }

    return result;
  }

  // Utility Methods
  clearSession(): void {
    if (typeof window !== 'undefined') {
      localStorage.removeItem('access_token');
      localStorage.removeItem('session_id');
      localStorage.removeItem('user_id');
      localStorage.removeItem('user_security');
    }
  }

  getStoredUserSecurity(): UserSecurityDTO | null {
    if (typeof window === 'undefined') return null;
    
    const userSecurityStr = localStorage.getItem('user_security');
    if (!userSecurityStr) return null;
    
    try {
      return JSON.parse(userSecurityStr);
    } catch {
      localStorage.removeItem('user_security');
      return null;
    }
  }

  getStoredUserId(): string | null {
    if (typeof window === 'undefined') return null;
    return localStorage.getItem('user_id');
  }

  getStoredSessionId(): string | null {
    if (typeof window === 'undefined') return null;
    return localStorage.getItem('session_id');
  }

  isAuthenticated(): boolean {
    if (typeof window === 'undefined') return false;
    
    const token = localStorage.getItem('access_token');
    const sessionId = localStorage.getItem('session_id');
    const userId = localStorage.getItem('user_id');
    
    return !!(token && sessionId && userId);
  }

  // Business Logic Helper Methods
  shouldRedirectToAdmin(userSecurity: UserSecurityDTO): boolean {
    return userSecurity.userType === 'ADMIN';
  }

  shouldRedirectToGeneral(userSecurity: UserSecurityDTO): boolean {
    return userSecurity.userType === 'USER' || userSecurity.userType === 'GUEST';
  }

  getRedirectPath(userSecurity: UserSecurityDTO): string {
    if (this.shouldRedirectToAdmin(userSecurity)) {
      return '/admin/dashboard'; // Equivalent to COADM01C
    } else if (this.shouldRedirectToGeneral(userSecurity)) {
      return '/dashboard'; // Equivalent to COMEN01C
    } else {
      return '/';
    }
  }

  isAccountLocked(userSecurity: UserSecurityDTO): boolean {
    return userSecurity.accountLocked || userSecurity.userStatus === 'LOCKED';
  }

  isPasswordExpired(userSecurity: UserSecurityDTO): boolean {
    return userSecurity.passwordExpired;
  }

  isAccountActive(userSecurity: UserSecurityDTO): boolean {
    return userSecurity.userStatus === 'ACTIVE' && !this.isAccountLocked(userSecurity);
  }

  canUserSignOn(userSecurity: UserSecurityDTO): { canSignOn: boolean; reason?: string } {
    if (!this.isAccountActive(userSecurity)) {
      return { canSignOn: false, reason: 'Account is not active' };
    }

    if (this.isAccountLocked(userSecurity)) {
      return { canSignOn: false, reason: 'Account is locked' };
    }

    if (userSecurity.userStatus === 'SUSPENDED') {
      return { canSignOn: false, reason: 'Account is suspended' };
    }

    return { canSignOn: true };
  }
}

export const userSecurityService = new UserSecurityService();