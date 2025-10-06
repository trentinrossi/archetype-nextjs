import { Token, UserCreate, UserLogin, UserResponse } from "@/types/auth";

const API_BASE_URL = '/api';

class AuthService {
  private isRefreshing = false;
  private refreshSubscribers: Array<(token: string) => void> = [];

  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  private async makeAuthenticatedRequest(url: string, options: RequestInit = {}): Promise<Response> {
    let response = await fetch(url, {
      ...options,
      headers: {
        ...this.getAuthHeaders(),
        ...options.headers,
      },
    });

    // If we get a 401, try to refresh the token
    if (response.status === 401 && !this.isRefreshing) {
      try {
        await this.handleTokenRefresh();
        // Retry the request with the new token
        response = await fetch(url, {
          ...options,
          headers: {
            ...this.getAuthHeaders(),
            ...options.headers,
          },
        });
      } catch (error) {
        // Refresh failed, redirect to login
        console.error('Token refresh failed during request:', error);
        this.removeToken();
        throw new Error('Session expired');
      }
    }

    return response;
  }

  private async handleTokenRefresh(): Promise<void> {
    if (this.isRefreshing) {
      // If already refreshing, wait for it to complete
      return new Promise((resolve) => {
        this.refreshSubscribers.push(() => resolve());
      });
    }

    this.isRefreshing = true;

    try {
      const tokenResponse = await this.refreshToken();
      this.setToken(tokenResponse.access_token);
      
      // Notify all waiting subscribers
      this.refreshSubscribers.forEach(callback => callback(tokenResponse.access_token));
      this.refreshSubscribers = [];
    } catch (error) {
      throw error;
    } finally {
      this.isRefreshing = false;
    }
  }

  async login(credentials: UserLogin): Promise<Token> {
    const response = await fetch(`${API_BASE_URL}/auth/login`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(credentials),
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.detail || 'Login failed');
    }

    return response.json();
  }

  async register(userData: UserCreate): Promise<UserResponse> {
    const response = await fetch(`${API_BASE_URL}/auth/register`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(userData),
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.detail || 'Registration failed');
    }

    return response.json();
  }

  async getCurrentUser(): Promise<UserResponse> {
    const response = await this.makeAuthenticatedRequest(`${API_BASE_URL}/auth/me`, {
      method: 'GET',
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.detail || 'Failed to get user info');
    }

    return response.json();
  }

  async refreshToken(): Promise<Token> {
    const response = await fetch(`${API_BASE_URL}/auth/refresh`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      if (response.status === 401) {
        // Refresh token is invalid or expired
        localStorage.removeItem('access_token');
        localStorage.removeItem('user');
        throw new Error('Session expired');
      }
      const error = await response.json();
      throw new Error(error.detail || 'Token refresh failed');
    }

    return response.json();
  }

  async listUsers(limit = 100, skip = 0): Promise<UserResponse[]> {
    const params = new URLSearchParams({
      limit: limit.toString(),
      skip: skip.toString(),
    });

    const response = await this.makeAuthenticatedRequest(`${API_BASE_URL}/auth/users?${params}`, {
      method: 'GET',
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.detail || 'Failed to list users');
    }

    return response.json();
  }

  async deleteUser(userId: string): Promise<{ message: string }> {
    const response = await this.makeAuthenticatedRequest(`${API_BASE_URL}/auth/users/${userId}`, {
      method: 'DELETE',
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.detail || 'Failed to delete user');
    }

    return response.json();
  }

  async deactivateUser(userId: string): Promise<{ message: string }> {
    const response = await this.makeAuthenticatedRequest(`${API_BASE_URL}/auth/users/${userId}/deactivate`, {
      method: 'PUT',
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.detail || 'Failed to deactivate user');
    }

    return response.json();
  }

  // Helper methods for token management
  setToken(token: string): void {
    localStorage.setItem('access_token', token);
  }

  getToken(): string | null {
    return localStorage.getItem('access_token');
  }

  removeToken(): void {
    localStorage.removeItem('access_token');
    localStorage.removeItem('user');
  }

  setUser(userResponse: UserResponse): void {
    localStorage.setItem('user', JSON.stringify(userResponse.user));
  }

  getUser(): UserResponse['user'] | null {
    const userStr = localStorage.getItem('user');
    if (!userStr) return null;
    
    try {
      const parsed = JSON.parse(userStr);
      
      if (parsed.message) {
        localStorage.removeItem('user');
        return null;
      }
      
      return parsed;
    } catch {
      localStorage.removeItem('user');
      return null;
    }
  }
}

export const authService = new AuthService();