import {
  UserCreateRequest,
  UserUpdateRequest,
  UserSignOnRequest,
  UserResponse,
  UserSignOnResponse,
  UserListResponse,
  Pageable,
  ApiErrorResponse,
  UserService as IUserService
} from '@/types/user';

class UserService implements IUserService {
  private baseUrl = '/api/users';

  private async handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
      let errorData: ApiErrorResponse;
      
      try {
        errorData = await response.json();
      } catch {
        errorData = {
          error: {
            message: `HTTP ${response.status}: ${response.statusText}`,
            code: 'HTTP_ERROR'
          },
          timestamp: new Date().toISOString(),
          path: response.url
        };
      }

      throw new Error(errorData.error.message || 'An error occurred');
    }

    if (response.status === 204) {
      return {} as T;
    }

    try {
      return await response.json();
    } catch {
      throw new Error('Invalid response format');
    }
  }

  private buildQueryString(params: Record<string, any>): string {
    const searchParams = new URLSearchParams();
    
    Object.entries(params).forEach(([key, value]) => {
      if (value !== undefined && value !== null && value !== '') {
        if (Array.isArray(value)) {
          value.forEach(item => searchParams.append(key, String(item)));
        } else {
          searchParams.append(key, String(value));
        }
      }
    });

    return searchParams.toString();
  }

  async getUsers(params?: Pageable): Promise<UserListResponse> {
    try {
      let url = this.baseUrl;
      
      if (params) {
        const queryParams: Record<string, any> = {};
        
        // Pagination parameters matching Pageable interface
        if (params.page !== undefined) queryParams.page = params.page;
        if (params.size !== undefined) queryParams.size = params.size;
        if (params.sort && params.sort.length > 0) {
          queryParams.sort = params.sort;
        }
        
        const queryString = this.buildQueryString(queryParams);
        if (queryString) {
          url += `?${queryString}`;
        }
      }

      const response = await fetch(url, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      });

      return await this.handleResponse<UserListResponse>(response);
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }
      throw new Error('Failed to fetch users');
    }
  }

  async getUserById(userId: string): Promise<UserResponse> {
    try {
      if (!userId || userId.trim() === '') {
        throw new Error('User ID is required');
      }

      const response = await fetch(`${this.baseUrl}/${encodeURIComponent(userId)}`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      });

      return await this.handleResponse<UserResponse>(response);
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }
      throw new Error('Failed to fetch user');
    }
  }

  async createUser(data: UserCreateRequest): Promise<UserResponse> {
    try {
      // Validate required fields based on API schema
      if (!data.userId || !data.firstName || !data.lastName || !data.password || !data.userType) {
        throw new Error('User ID, first name, last name, password, and user type are required');
      }

      const response = await fetch(this.baseUrl, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(data),
      });

      return await this.handleResponse<UserResponse>(response);
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }
      throw new Error('Failed to create user');
    }
  }

  async updateUser(userId: string, data: UserUpdateRequest): Promise<UserResponse> {
    try {
      if (!userId || userId.trim() === '') {
        throw new Error('User ID is required');
      }

      if (!data || Object.keys(data).length === 0) {
        throw new Error('Update data is required');
      }

      const response = await fetch(`${this.baseUrl}/${encodeURIComponent(userId)}`, {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(data),
      });

      return await this.handleResponse<UserResponse>(response);
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }
      throw new Error('Failed to update user');
    }
  }

  async deleteUser(userId: string): Promise<void> {
    try {
      if (!userId || userId.trim() === '') {
        throw new Error('User ID is required');
      }

      const response = await fetch(`${this.baseUrl}/${encodeURIComponent(userId)}`, {
        method: 'DELETE',
        headers: {
          'Content-Type': 'application/json',
        },
      });

      await this.handleResponse<void>(response);
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }
      throw new Error('Failed to delete user');
    }
  }

  async signIn(data: UserSignOnRequest): Promise<UserSignOnResponse> {
    try {
      if (!data.userId || !data.password) {
        throw new Error('User ID and password are required');
      }

      const response = await fetch(`${this.baseUrl}/signin`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(data),
      });

      return await this.handleResponse<UserSignOnResponse>(response);
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }
      throw new Error('Failed to sign in');
    }
  }

  // Additional utility methods for common operations
  async searchUsersFromId(startUserId: string, params?: Pageable): Promise<UserListResponse> {
    try {
      let url = this.baseUrl;
      const queryParams: Record<string, any> = {};
      
      // Add pagination parameters
      if (params?.page !== undefined) queryParams.page = params.page;
      if (params?.size !== undefined) queryParams.size = params.size;
      if (params?.sort && params.sort.length > 0) {
        queryParams.sort = params.sort;
      }
      
      // Add search parameter for starting user ID
      if (startUserId && startUserId.trim() !== '') {
        queryParams.startUserId = startUserId;
      }
      
      const queryString = this.buildQueryString(queryParams);
      if (queryString) {
        url += `?${queryString}`;
      }

      const response = await fetch(url, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      });

      return await this.handleResponse<UserListResponse>(response);
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }
      throw new Error('Failed to search users');
    }
  }

  // Batch operations
  async getUsersByIds(userIds: string[]): Promise<UserResponse[]> {
    try {
      if (!userIds || userIds.length === 0) {
        return [];
      }

      const promises = userIds.map(userId => this.getUserById(userId));
      const results = await Promise.allSettled(promises);
      
      return results
        .filter((result): result is PromiseFulfilledResult<UserResponse> => result.status === 'fulfilled')
        .map(result => result.value);
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }
      throw new Error('Failed to fetch users by IDs');
    }
  }

  // Helper methods for user type filtering
  async getAdminUsers(params?: Pageable): Promise<UserListResponse> {
    // This would need to be implemented on the backend to filter by userType
    // For now, we'll get all users and filter client-side
    const result = await this.getUsers(params);
    return {
      ...result,
      users: result.users.filter(user => user.userType === 'A')
    };
  }

  async getRegularUsers(params?: Pageable): Promise<UserListResponse> {
    // This would need to be implemented on the backend to filter by userType
    // For now, we'll get all users and filter client-side
    const result = await this.getUsers(params);
    return {
      ...result,
      users: result.users.filter(user => user.userType === 'R')
    };
  }

  // Validation helpers
  validateUserId(userId: string): boolean {
    return userId && userId.trim().length > 0 && userId.length <= 8;
  }

  validateUserData(data: Partial<UserCreateRequest>): string[] {
    const errors: string[] = [];
    
    if (!data.userId || data.userId.trim() === '') {
      errors.push('User ID is required');
    } else if (data.userId.length > 8) {
      errors.push('User ID must be 8 characters or less');
    }
    
    if (!data.firstName || data.firstName.trim() === '') {
      errors.push('First name is required');
    } else if (data.firstName.length > 20) {
      errors.push('First name must be 20 characters or less');
    }
    
    if (!data.lastName || data.lastName.trim() === '') {
      errors.push('Last name is required');
    } else if (data.lastName.length > 20) {
      errors.push('Last name must be 20 characters or less');
    }
    
    if (!data.password || data.password.trim() === '') {
      errors.push('Password is required');
    } else if (data.password.length > 8) {
      errors.push('Password must be 8 characters or less');
    }
    
    if (!data.userType || data.userType.trim() === '') {
      errors.push('User type is required');
    } else if (!['A', 'R'].includes(data.userType)) {
      errors.push('User type must be A (Admin) or R (Regular)');
    }
    
    return errors;
  }
}

// Export singleton instance
export const userService = new UserService();

// Export class for testing or custom instances
export { UserService };

// Default export
export default userService;