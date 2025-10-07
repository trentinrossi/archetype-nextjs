import { 
  SignonRequestDTO, 
  SignonResponseDTO, 
  UserSecurityDTO, 
  CreateUserSecurityRequest,
  UpdateUserSecurityRequest,
  ChangePasswordRequest,
  UserListResponse,
  UserListRequest,
  ApiResponse,
  AuthError,
  AUTH_ERROR_CODES
} from '@/types/user';

class UserService {
  private baseUrl = '/api';

  async signon(credentials: SignonRequestDTO): Promise<SignonResponseDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/auth/signon`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(credentials),
        credentials: 'include',
      });

      const data = await response.json();

      if (!response.ok) {
        throw new Error(data.message || 'Authentication failed');
      }

      return data;
    } catch (error) {
      console.error('Signon error:', error);
      throw error;
    }
  }

  async exit(): Promise<void> {
    try {
      const response = await fetch(`${this.baseUrl}/auth/exit`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        credentials: 'include',
      });

      if (!response.ok) {
        const data = await response.json();
        throw new Error(data.message || 'Exit failed');
      }
    } catch (error) {
      console.error('Exit error:', error);
      throw error;
    }
  }

  async validateSession(): Promise<boolean> {
    try {
      const response = await fetch(`${this.baseUrl}/auth/validate`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        credentials: 'include',
      });

      return response.ok;
    } catch (error) {
      console.error('Session validation error:', error);
      return false;
    }
  }

  async handleInvalidKey(keyPressed: string): Promise<ApiResponse> {
    try {
      const response = await fetch(`${this.baseUrl}/auth/invalid-key`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ keyPressed }),
        credentials: 'include',
      });

      const data = await response.json();

      if (!response.ok) {
        throw new Error(data.message || 'Invalid key handling failed');
      }

      return data;
    } catch (error) {
      console.error('Invalid key handling error:', error);
      throw error;
    }
  }

  async getCurrentUser(): Promise<UserSecurityDTO | null> {
    try {
      const response = await fetch(`${this.baseUrl}/auth/me`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
        credentials: 'include',
      });

      if (!response.ok) {
        if (response.status === 401) {
          return null;
        }
        const data = await response.json();
        throw new Error(data.message || 'Failed to get current user');
      }

      const data = await response.json();
      return data.user;
    } catch (error) {
      console.error('Get current user error:', error);
      return null;
    }
  }

  async createUser(userData: CreateUserSecurityRequest): Promise<UserSecurityDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/users`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(userData),
        credentials: 'include',
      });

      const data = await response.json();

      if (!response.ok) {
        throw new Error(data.message || 'Failed to create user');
      }

      return data.user;
    } catch (error) {
      console.error('Create user error:', error);
      throw error;
    }
  }

  async updateUser(userId: string, userData: UpdateUserSecurityRequest): Promise<UserSecurityDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${userId}`, {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(userData),
        credentials: 'include',
      });

      const data = await response.json();

      if (!response.ok) {
        throw new Error(data.message || 'Failed to update user');
      }

      return data.user;
    } catch (error) {
      console.error('Update user error:', error);
      throw error;
    }
  }

  async deleteUser(userId: string): Promise<void> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${userId}`, {
        method: 'DELETE',
        headers: {
          'Content-Type': 'application/json',
        },
        credentials: 'include',
      });

      if (!response.ok) {
        const data = await response.json();
        throw new Error(data.message || 'Failed to delete user');
      }
    } catch (error) {
      console.error('Delete user error:', error);
      throw error;
    }
  }

  async getUser(userId: string): Promise<UserSecurityDTO> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${userId}`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
        credentials: 'include',
      });

      const data = await response.json();

      if (!response.ok) {
        throw new Error(data.message || 'Failed to get user');
      }

      return data.user;
    } catch (error) {
      console.error('Get user error:', error);
      throw error;
    }
  }

  async getUsers(params: UserListRequest = {}): Promise<UserListResponse> {
    try {
      const searchParams = new URLSearchParams();
      
      if (params.page) searchParams.append('page', params.page.toString());
      if (params.pageSize) searchParams.append('pageSize', params.pageSize.toString());
      if (params.filter) searchParams.append('filter', params.filter);
      if (params.sortBy) searchParams.append('sortBy', params.sortBy);
      if (params.sortOrder) searchParams.append('sortOrder', params.sortOrder);

      const response = await fetch(`${this.baseUrl}/users?${searchParams.toString()}`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
        credentials: 'include',
      });

      const data = await response.json();

      if (!response.ok) {
        throw new Error(data.message || 'Failed to get users');
      }

      return data;
    } catch (error) {
      console.error('Get users error:', error);
      throw error;
    }
  }

  async changePassword(userId: string, passwordData: ChangePasswordRequest): Promise<void> {
    try {
      const response = await fetch(`${this.baseUrl}/users/${userId}/change-password`, {
        method: 'PATCH',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ newPassword: passwordData.newPassword }),
        credentials: 'include',
      });

      if (!response.ok) {
        const data = await response.json();
        throw new Error(data.message || 'Failed to change password');
      }
    } catch (error) {
      console.error('Change password error:', error);
      throw error;
    }
  }

  validateCredentials(credentials: SignonRequestDTO): string[] {
    const errors: string[] = [];

    if (!credentials.userId || credentials.userId.trim() === '') {
      errors.push('User ID can NOT be empty...');
    } else if (credentials.userId.length > 8) {
      errors.push('User ID must be 8 characters or less');
    }

    if (!credentials.password || credentials.password.trim() === '') {
      errors.push('Password can NOT be empty...');
    } else if (credentials.password.length > 8) {
      errors.push('Password must be 8 characters or less');
    }

    return errors;
  }

  validateUserData(userData: CreateUserSecurityRequest): string[] {
    const errors: string[] = [];

    if (!userData.userId || userData.userId.trim() === '') {
      errors.push('User ID can NOT be empty...');
    } else if (userData.userId.length > 8) {
      errors.push('User ID must be 8 characters or less');
    }

    if (!userData.firstName || userData.firstName.trim() === '') {
      errors.push('First Name can NOT be empty...');
    } else if (userData.firstName.length > 20) {
      errors.push('First Name must be 20 characters or less');
    }

    if (!userData.lastName || userData.lastName.trim() === '') {
      errors.push('Last Name can NOT be empty...');
    } else if (userData.lastName.length > 20) {
      errors.push('Last Name must be 20 characters or less');
    }

    if (!userData.password || userData.password.trim() === '') {
      errors.push('Password can NOT be empty...');
    } else if (userData.password.length > 8) {
      errors.push('Password must be 8 characters or less');
    }

    if (!userData.userType || (userData.userType !== 'A' && userData.userType !== 'G')) {
      errors.push('User Type can NOT be empty...');
    }

    return errors;
  }

  formatUserForDisplay(user: UserSecurityDTO): string {
    return `${user.firstName} ${user.lastName} (${user.userId})`;
  }

  getUserTypeLabel(userType: 'A' | 'G'): string {
    return userType === 'A' ? 'Administrator' : 'General User';
  }

  canUserPerformAction(userType: 'A' | 'G', action: 'create' | 'update' | 'delete' | 'view'): boolean {
    if (userType === 'A') {
      return true;
    }
    
    return action === 'view';
  }

  sanitizeUserId(userId: string): string {
    return userId.toUpperCase().trim().replace(/[^A-Z0-9]/g, '');
  }

  sanitizeName(name: string): string {
    return name.trim().replace(/[^A-Za-z\s]/g, '');
  }
}

export const userService = new UserService();
export default userService;