import { User, CreateUserRequest, UpdateUserRequest, PaginatedUsersResponse, UserFilterParams } from '@/types/user';

const API_BASE_URL = '/api';

interface LoginRequest {
  userId: string;
  password: string;
}

interface LoginResponse {
  userId: string;
  firstName: string;
  lastName: string;
  userType: string;
  message: string;
}

interface ApiResponse<T> {
  data?: T;
  message?: string;
  error?: string;
}

class UserService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async login(userId: string, password: string): Promise<LoginResponse> {
    const response = await fetch(`${API_BASE_URL}/auth/login`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ userId, password } as LoginRequest),
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Login failed');
    }

    const data: LoginResponse = await response.json();
    
    if (data.userId) {
      localStorage.setItem('user', JSON.stringify(data));
      localStorage.setItem('access_token', 'authenticated');
    }

    return data;
  }

  async getUserList(params?: UserFilterParams): Promise<PaginatedUsersResponse> {
    const queryParams = new URLSearchParams();
    
    if (params?.page !== undefined) {
      queryParams.append('page', params.page.toString());
    }
    if (params?.size !== undefined) {
      queryParams.append('size', params.size.toString());
    }
    if (params?.sort) {
      queryParams.append('sort', params.sort);
    }
    if (params?.userType) {
      queryParams.append('userType', params.userType);
    }
    if (params?.firstName) {
      queryParams.append('firstName', params.firstName);
    }
    if (params?.lastName) {
      queryParams.append('lastName', params.lastName);
    }

    const url = `${API_BASE_URL}/users${queryParams.toString() ? `?${queryParams.toString()}` : ''}`;
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch users');
    }

    return response.json();
  }

  async getUserById(userId: string): Promise<User> {
    const response = await fetch(`${API_BASE_URL}/users/${userId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (response.status === 404) {
      throw new Error('User not found');
    }

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch user');
    }

    return response.json();
  }

  async createUser(data: CreateUserRequest): Promise<{ user: User; message: string }> {
    const response = await fetch(`${API_BASE_URL}/users`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });

    if (response.status === 400) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'User already exists');
    }

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to create user');
    }

    return response.json();
  }

  async updateUser(userId: string, data: UpdateUserRequest): Promise<{ user: User; message: string }> {
    const response = await fetch(`${API_BASE_URL}/users/${userId}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });

    if (response.status === 400) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Invalid user data');
    }

    if (response.status === 404) {
      throw new Error('User not found');
    }

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to update user');
    }

    return response.json();
  }

  async deleteUser(userId: string): Promise<{ message: string }> {
    const response = await fetch(`${API_BASE_URL}/users/${userId}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });

    if (response.status === 400) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Cannot delete user');
    }

    if (response.status === 404) {
      throw new Error('User not found');
    }

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to delete user');
    }

    return response.json();
  }

  logout(): void {
    localStorage.removeItem('access_token');
    localStorage.removeItem('user');
  }

  getCurrentUser(): LoginResponse | null {
    const userStr = localStorage.getItem('user');
    if (!userStr) return null;
    
    try {
      return JSON.parse(userStr) as LoginResponse;
    } catch {
      return null;
    }
  }

  isAuthenticated(): boolean {
    return !!localStorage.getItem('access_token');
  }
}

export const userService = new UserService();
