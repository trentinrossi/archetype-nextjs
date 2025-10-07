import { 
  SignonRequestDTO, 
  SignonResponseDTO, 
  UserSecurityDTO, 
  CreateUserSecurityRequest, 
  UpdateUserSecurityRequest, 
  ChangePasswordRequest,
  ValidationResponseDTO,
  PaginationParams,
  ApiResponse
} from '@/types/user';

class UserService {
  private baseUrl = '/api';

  // Authentication services
  async signon(credentials: SignonRequestDTO): Promise<SignonResponseDTO> {
    const response = await fetch(`${this.baseUrl}/auth/signon`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(credentials),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.message || 'Authentication failed');
    }

    return response.json();
  }

  async exit(): Promise<{ success: boolean; message: string }> {
    const response = await fetch(`${this.baseUrl}/auth/exit`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
    });

    if (!response.ok) {
      throw new Error('Exit failed');
    }

    return response.json();
  }

  async handleInvalidKey(): Promise<{ success: boolean; message: string }> {
    const response = await fetch(`${this.baseUrl}/auth/invalid-key`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
    });

    if (!response.ok) {
      throw new Error('Invalid key handling failed');
    }

    return response.json();
  }

  async validateCredentials(credentials: SignonRequestDTO): Promise<ValidationResponseDTO> {
    const response = await fetch(`${this.baseUrl}/auth/validate`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(credentials),
    });

    if (!response.ok) {
      const errorData = await response.json();
      return {
        valid: false,
        message: errorData.message || 'Validation failed'
      };
    }

    return response.json();
  }

  // User management services
  async getAllUsers(params?: PaginationParams): Promise<{
    users: UserSecurityDTO[];
    pagination: {
      page: number;
      size: number;
      total: number;
      totalPages: number;
    };
  }> {
    const searchParams = new URLSearchParams();
    if (params?.page !== undefined) searchParams.set('page', params.page.toString());
    if (params?.size !== undefined) searchParams.set('size', params.size.toString());
    if (params?.sort) searchParams.set('sort', params.sort);

    const response = await fetch(`${this.baseUrl}/users?${searchParams}`, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      },
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to fetch users');
    }

    return response.json();
  }

  async getUserById(userId: string): Promise<UserSecurityDTO> {
    const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}`, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      },
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'User not found');
    }

    return response.json();
  }

  async createUser(userData: CreateUserSecurityRequest): Promise<UserSecurityDTO> {
    const response = await fetch(`${this.baseUrl}/users`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(userData),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to create user');
    }

    return response.json();
  }

  async updateUser(userId: string, userData: UpdateUserSecurityRequest): Promise<UserSecurityDTO> {
    const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}`, {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(userData),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to update user');
    }

    return response.json();
  }

  async deleteUser(userId: string): Promise<void> {
    const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}`, {
      method: 'DELETE',
      headers: {
        'Content-Type': 'application/json',
      },
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to delete user');
    }
  }

  async activateUser(userId: string): Promise<{ success: boolean; message: string; user: UserSecurityDTO }> {
    const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}/activate`, {
      method: 'PATCH',
      headers: {
        'Content-Type': 'application/json',
      },
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to activate user');
    }

    return response.json();
  }

  async deactivateUser(userId: string): Promise<{ success: boolean; message: string; user: UserSecurityDTO }> {
    const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}/deactivate`, {
      method: 'PATCH',
      headers: {
        'Content-Type': 'application/json',
      },
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to deactivate user');
    }

    return response.json();
  }

  async changePassword(userId: string, passwordData: ChangePasswordRequest): Promise<{ success: boolean; message: string }> {
    const response = await fetch(`${this.baseUrl}/users/${encodeURIComponent(userId)}/change-password`, {
      method: 'PATCH',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(passwordData),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to change password');
    }

    return response.json();
  }

  // Utility methods for COBOL business logic
  validateUserId(userId: string): { valid: boolean; message: string } {
    if (!userId || userId.trim() === '') {
      return { valid: false, message: 'Please enter User ID' };
    }
    if (userId.length > 8) {
      return { valid: false, message: 'User ID must be 8 characters or less' };
    }
    return { valid: true, message: '' };
  }

  validatePassword(password: string): { valid: boolean; message: string } {
    if (!password || password.trim() === '') {
      return { valid: false, message: 'Please enter Password' };
    }
    if (password.length > 8) {
      return { valid: false, message: 'Password must be 8 characters or less' };
    }
    return { valid: true, message: '' };
  }

  validateUserType(userType: string): { valid: boolean; message: string } {
    if (!userType || userType.trim() === '') {
      return { valid: false, message: 'User Type can NOT be empty...' };
    }
    if (!['ADMIN', 'GENERAL', 'A', 'U'].includes(userType.toUpperCase())) {
      return { valid: false, message: 'Invalid user type' };
    }
    return { valid: true, message: '' };
  }

  validateFirstName(firstName: string): { valid: boolean; message: string } {
    if (!firstName || firstName.trim() === '') {
      return { valid: false, message: 'First Name can NOT be empty...' };
    }
    if (firstName.length > 20) {
      return { valid: false, message: 'First Name must be 20 characters or less' };
    }
    return { valid: true, message: '' };
  }

  validateLastName(lastName: string): { valid: boolean; message: string } {
    if (!lastName || lastName.trim() === '') {
      return { valid: false, message: 'Last Name can NOT be empty...' };
    }
    if (lastName.length > 20) {
      return { valid: false, message: 'Last Name must be 20 characters or less' };
    }
    return { valid: true, message: '' };
  }

  // Format user ID to uppercase (COBOL business logic)
  formatUserId(userId: string): string {
    return userId.toUpperCase().trim();
  }

  // Get redirect program based on user type (COBOL business logic)
  getRedirectProgram(userType: 'ADMIN' | 'GENERAL'): string {
    return userType === 'ADMIN' ? 'COADM01C' : 'COMEN01C';
  }
}

export const userService = new UserService();