import { User, CreateUserRequest, UpdateUserRequest, LoginRequest, LoginResponse } from '@/types/user';

const API_BASE_URL = '/api';

class UserService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getUsers(page: number = 0, size: number = 20, sort?: string): Promise<any> {
    let url = `${API_BASE_URL}/users?page=${page}&size=${size}`;
    if (sort) {
      url += `&sort=${sort}`;
    }
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch users');
    }
    return response.json();
  }

  async getUserById(id: number): Promise<User> {
    const response = await fetch(`${API_BASE_URL}/users/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user');
    }
    return response.json();
  }

  async getUserByUserId(userId: string): Promise<User> {
    const response = await fetch(`${API_BASE_URL}/users/by-user-id/${userId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user');
    }
    return response.json();
  }

  async createUser(data: CreateUserRequest): Promise<User> {
    const response = await fetch(`${API_BASE_URL}/users`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to create user');
    }
    return response.json();
  }

  async updateUser(id: number, data: UpdateUserRequest): Promise<User> {
    const response = await fetch(`${API_BASE_URL}/users/${id}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to update user');
    }
    return response.json();
  }

  async deleteUser(id: number): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/users/${id}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete user');
    }
  }

  async login(credentials: LoginRequest): Promise<LoginResponse> {
    const response = await fetch(`${API_BASE_URL}/users/login`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(credentials),
    });
    
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.message || 'Failed to authenticate');
    }
    
    const data = await response.json();
    
    if (data.sessionToken) {
      localStorage.setItem('access_token', data.sessionToken);
      localStorage.setItem('user', JSON.stringify(data.user));
    }
    
    return data;
  }

  async checkAdminStatus(userId: string): Promise<boolean> {
    const response = await fetch(`${API_BASE_URL}/users/by-user-id/${userId}/admin-check`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    return response.ok;
  }

  async searchUsers(searchTerm: string, page: number = 0, size: number = 20): Promise<any> {
    const url = `${API_BASE_URL}/users/search?searchTerm=${encodeURIComponent(searchTerm)}&page=${page}&size=${size}`;
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to search users');
    }
    return response.json();
  }

  async filterUsersByType(userType: string, page: number = 0, size: number = 20): Promise<any> {
    const url = `${API_BASE_URL}/users/filter-by-type?userType=${userType}&page=${page}&size=${size}`;
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to filter users');
    }
    return response.json();
  }

  logout(): void {
    localStorage.removeItem('access_token');
    localStorage.removeItem('user');
  }

  getCurrentUser(): any {
    const userStr = localStorage.getItem('user');
    return userStr ? JSON.parse(userStr) : null;
  }

  isAuthenticated(): boolean {
    return !!localStorage.getItem('access_token');
  }
}

export const userService = new UserService();
