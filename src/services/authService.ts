// src/services/authService.ts
import { 
  SignonRequestDTO, 
  SignonResponseDTO, 
  UserSecurityDTO, 
  ChangePasswordRequest,
  ValidationResponseDTO,
  ApiResponse,
  UserSession,
  PasswordValidationResult,
  PasswordPolicy,
  ValidationError
} from '@/types/userSecurity';

const API_BASE_URL = '/api';

interface AuthHeaders {
  'Content-Type': string;
  'Authorization'?: string;
}

class AuthService {
  private token: string | null = null;
  private refreshToken: string | null = null;
  private tokenExpiry: Date | null = null;

  constructor() {
    // Initialize tokens from localStorage if available (client-side only)
    if (typeof window !== 'undefined') {
      this.token = localStorage.getItem('auth_token');
      this.refreshToken = localStorage.getItem('refresh_token');
      const expiry = localStorage.getItem('token_expiry');
      this.tokenExpiry = expiry ? new Date(expiry) : null;
    }
  }

  private getAuthHeaders(): AuthHeaders {
    const headers: AuthHeaders = {
      'Content-Type': 'application/json',
    };

    if (this.token) {
      headers['Authorization'] = `Bearer ${this.token}`;
    }

    return headers;
  }

  private async handleResponse<T>(response: Response): Promise<ApiResponse<T>> {
    const data = await response.json().catch(() => ({}));
    
    if (!response.ok) {
      throw new Error(
        data.message || 
        data.detail?.[0]?.msg || 
        `HTTP ${response.status}: ${response.statusText}`
      );
    }

    return {
      success: true,
      data: data.data || data,
      message: data.message,
      errors: data.errors,
      timestamp: new Date().toISOString()
    };
  }

  private setAuthTokens(token: string, refreshToken: string, expiresAt: string): void {
    this.token = token;
    this.refreshToken = refreshToken;
    this.tokenExpiry = new Date(expiresAt);

    if (typeof window !== 'undefined') {
      localStorage.setItem('auth_token', token);
      localStorage.setItem('refresh_token', refreshToken);
      localStorage.setItem('token_expiry', expiresAt);
    }
  }

  private clearAuthTokens(): void {
    this.token = null;
    this.refreshToken = null;
    this.tokenExpiry = null;

    if (typeof window !== 'undefined') {
      localStorage.removeItem('auth_token');
      localStorage.removeItem('refresh_token');
      localStorage.removeItem('token_expiry');
      localStorage.removeItem('current_user');
    }
  }

  private isTokenExpired(): boolean {
    if (!this.tokenExpiry) return true;
    return new Date() >= this.tokenExpiry;
  }

  private async refreshAuthToken(): Promise<boolean> {
    if (!this.refreshToken) return false;

    try {
      const response = await fetch(`${API_BASE_URL}/auth/refresh`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ refreshToken: this.refreshToken }),
      });

      if (response.ok) {
        const data = await response.json();
        this.setAuthTokens(data.token, data.refreshToken, data.expiresAt);
        return true;
      }
    } catch (error) {
      console.error('Token refresh failed:', error);
    }

    this.clearAuthTokens();
    return false;
  }

  async ensureValidToken(): Promise<boolean> {
    if (!this.token) return false;
    
    if (this.isTokenExpired()) {
      return await this.refreshAuthToken();
    }
    
    return true;
  }

  // COSGN00C Business Logic Implementation
  async signon(credentials: SignonRequestDTO): Promise<SignonResponseDTO> {
    try {
      // Validate input
      const validation = this.validateSignonInput(credentials);
      if (!validation.isValid) {
        return {
          success: false,
          message: 'Invalid input provided',
          errors: validation.errors
        };
      }

      const response = await fetch(`${API_BASE_URL}/auth/signon`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          userId: credentials.userId.toUpperCase().trim(),
          password: credentials.password,
          rememberMe: credentials.rememberMe || false
        }),
      });

      const result = await this.handleResponse<SignonResponseDTO>(response);
      const signonData = result.data as SignonResponseDTO;

      if (signonData.success && signonData.token && signonData.user) {
        // Store authentication tokens
        this.setAuthTokens(
          signonData.token, 
          signonData.refreshToken || '', 
          signonData.expiresAt || new Date(Date.now() + 24 * 60 * 60 * 1000).toISOString()
        );

        // Store current user info
        if (typeof window !== 'undefined') {
          localStorage.setItem('current_user', JSON.stringify(signonData.user));
        }

        // Update last login timestamp
        await this.updateLastLogin(signonData.user.id);

        return {
          success: true,
          user: signonData.user,
          token: signonData.token,
          refreshToken: signonData.refreshToken,
          expiresAt: signonData.expiresAt,
          message: `Welcome, ${signonData.user.firstName} ${signonData.user.lastName}!`
        };
      }

      return signonData;
    } catch (error) {
      console.error('Signon error:', error);
      return {
        success: false,
        message: error instanceof Error ? error.message : 'Authentication failed. Please try again.',
        errors: [{ field: 'general', message: 'System error occurred during authentication' }]
      };
    }
  }

  async signout(): Promise<{ success: boolean; message: string }> {
    try {
      if (this.token) {
        await fetch(`${API_BASE_URL}/auth/signout`, {
          method: 'POST',
          headers: this.getAuthHeaders(),
        });
      }

      this.clearAuthTokens();

      return {
        success: true,
        message: 'Thank you for using the system. Goodbye!'
      };
    } catch (error) {
      console.error('Signout error:', error);
      this.clearAuthTokens(); // Clear tokens even if API call fails
      return {
        success: true,
        message: 'You have been signed out.'
      };
    }
  }

  async getCurrentUser(): Promise<UserSecurityDTO | null> {
    try {
      if (!await this.ensureValidToken()) {
        return null;
      }

      // Try to get from localStorage first
      if (typeof window !== 'undefined') {
        const storedUser = localStorage.getItem('current_user');
        if (storedUser) {
          return JSON.parse(storedUser);
        }
      }

      const response = await fetch(`${API_BASE_URL}/auth/me`, {
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<UserSecurityDTO>(response);
      return result.data as UserSecurityDTO;
    } catch (error) {
      console.error('Get current user error:', error);
      return null;
    }
  }

  async changePassword(request: ChangePasswordRequest): Promise<ValidationResponseDTO> {
    try {
      if (!await this.ensureValidToken()) {
        throw new Error('Authentication required');
      }

      // Validate password change request
      const validation = this.validatePasswordChange(request);
      if (!validation.isValid) {
        return validation;
      }

      const response = await fetch(`${API_BASE_URL}/auth/change-password`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(request),
      });

      const result = await this.handleResponse<ValidationResponseDTO>(response);
      return result.data as ValidationResponseDTO;
    } catch (error) {
      console.error('Change password error:', error);
      return {
        isValid: false,
        errors: [{ field: 'general', message: error instanceof Error ? error.message : 'Password change failed' }]
      };
    }
  }

  async validatePassword(password: string): Promise<PasswordValidationResult> {
    const policy: PasswordPolicy = {
      minLength: 8,
      maxLength: 20,
      requireUppercase: true,
      requireLowercase: true,
      requireNumbers: true,
      requireSpecialChars: false,
      forbiddenPasswords: ['password', '12345678', 'qwerty', 'admin']
    };

    const requirements = {
      length: password.length >= policy.minLength && password.length <= policy.maxLength,
      uppercase: policy.requireUppercase ? /[A-Z]/.test(password) : true,
      lowercase: policy.requireLowercase ? /[a-z]/.test(password) : true,
      numbers: policy.requireNumbers ? /\d/.test(password) : true,
      specialChars: policy.requireSpecialChars ? /[!@#$%^&*(),.?":{}|<>]/.test(password) : true,
      notForbidden: !policy.forbiddenPasswords.includes(password.toLowerCase())
    };

    const feedback: string[] = [];
    let score = 0;

    if (!requirements.length) {
      feedback.push(`Password must be between ${policy.minLength} and ${policy.maxLength} characters`);
    } else {
      score += 20;
    }

    if (!requirements.uppercase) {
      feedback.push('Password must contain at least one uppercase letter');
    } else {
      score += 20;
    }

    if (!requirements.lowercase) {
      feedback.push('Password must contain at least one lowercase letter');
    } else {
      score += 20;
    }

    if (!requirements.numbers) {
      feedback.push('Password must contain at least one number');
    } else {
      score += 20;
    }

    if (!requirements.specialChars && policy.requireSpecialChars) {
      feedback.push('Password must contain at least one special character');
    } else if (policy.requireSpecialChars) {
      score += 20;
    }

    if (!requirements.notForbidden) {
      feedback.push('Password is too common. Please choose a different password');
      score = 0;
    }

    const isValid = Object.values(requirements).every(req => req === true);

    return {
      isValid,
      score: isValid ? score : Math.min(score, 60),
      feedback,
      requirements
    };
  }

  async getUserSessions(): Promise<UserSession[]> {
    try {
      if (!await this.ensureValidToken()) {
        throw new Error('Authentication required');
      }

      const response = await fetch(`${API_BASE_URL}/auth/sessions`, {
        headers: this.getAuthHeaders(),
      });

      const result = await this.handleResponse<UserSession[]>(response);
      return result.data as UserSession[];
    } catch (error) {
      console.error('Get user sessions error:', error);
      return [];
    }
  }

  async revokeSession(sessionId: string): Promise<boolean> {
    try {
      if (!await this.ensureValidToken()) {
        throw new Error('Authentication required');
      }

      const response = await fetch(`${API_BASE_URL}/auth/sessions/${sessionId}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });

      return response.ok;
    } catch (error) {
      console.error('Revoke session error:', error);
      return false;
    }
  }

  private validateSignonInput(credentials: SignonRequestDTO): ValidationResponseDTO {
    const errors: ValidationError[] = [];

    // Validate User ID (SEC-USR-ID: 8 chars)
    if (!credentials.userId || credentials.userId.trim().length === 0) {
      errors.push({ field: 'userId', message: 'User ID is required' });
    } else if (credentials.userId.trim().length > 8) {
      errors.push({ field: 'userId', message: 'User ID must not exceed 8 characters' });
    } else if (!/^[A-Za-z0-9]+$/.test(credentials.userId.trim())) {
      errors.push({ field: 'userId', message: 'User ID must contain only letters and numbers' });
    }

    // Validate Password (SEC-USR-PWD: 8 chars)
    if (!credentials.password || credentials.password.length === 0) {
      errors.push({ field: 'password', message: 'Password is required' });
    } else if (credentials.password.length > 20) {
      errors.push({ field: 'password', message: 'Password must not exceed 20 characters' });
    }

    return {
      isValid: errors.length === 0,
      errors
    };
  }

  private validatePasswordChange(request: ChangePasswordRequest): ValidationResponseDTO {
    const errors: ValidationError[] = [];

    if (!request.currentPassword) {
      errors.push({ field: 'currentPassword', message: 'Current password is required' });
    }

    if (!request.newPassword) {
      errors.push({ field: 'newPassword', message: 'New password is required' });
    }

    if (!request.confirmPassword) {
      errors.push({ field: 'confirmPassword', message: 'Password confirmation is required' });
    }

    if (request.newPassword && request.confirmPassword && request.newPassword !== request.confirmPassword) {
      errors.push({ field: 'confirmPassword', message: 'Passwords do not match' });
    }

    if (request.currentPassword && request.newPassword && request.currentPassword === request.newPassword) {
      errors.push({ field: 'newPassword', message: 'New password must be different from current password' });
    }

    return {
      isValid: errors.length === 0,
      errors
    };
  }

  private async updateLastLogin(userId: string): Promise<void> {
    try {
      await fetch(`${API_BASE_URL}/auth/update-last-login`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify({ userId }),
      });
    } catch (error) {
      console.error('Update last login error:', error);
      // Non-critical error, don't throw
    }
  }

  // Utility methods
  isAuthenticated(): boolean {
    return this.token !== null && !this.isTokenExpired();
  }

  getToken(): string | null {
    return this.token;
  }

  getTokenExpiry(): Date | null {
    return this.tokenExpiry;
  }

  // Handle PF3 key functionality (Exit with thank you message)
  handleExitKey(): { success: boolean; message: string } {
    return {
      success: true,
      message: 'Thank you for using the system. Have a great day!'
    };
  }

  // Handle invalid key functionality
  handleInvalidKey(keyPressed: string): { success: boolean; message: string } {
    return {
      success: false,
      message: `Invalid key '${keyPressed}' pressed. Please use ENTER to sign on or PF3 to exit.`
    };
  }

  // Simulate COSGN00C key handling
  async handleKeyPress(key: string, formData?: SignonRequestDTO): Promise<SignonResponseDTO | { success: boolean; message: string }> {
    switch (key.toLowerCase()) {
      case 'enter':
        if (!formData) {
          return {
            success: false,
            message: 'Please enter your User ID and Password before pressing ENTER.'
          };
        }
        return await this.signon(formData);
      
      case 'pf3':
      case 'f3':
        await this.signout();
        return this.handleExitKey();
      
      default:
        return this.handleInvalidKey(key);
    }
  }
}

export const authService = new AuthService();
export default authService;