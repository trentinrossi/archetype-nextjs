import { AdminUser, CreateAdminUserRequest, UpdateAdminUserRequest } from '@/types/admin-user';

const API_BASE_URL = '/api';

class AdminUserService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getAdminUsers(page: number = 0, size: number = 20, sort?: string): Promise<any> {
    const queryParams = new URLSearchParams({
      page: page.toString(),
      size: size.toString(),
      ...(sort && { sort }),
    });
    
    const response = await fetch(`${API_BASE_URL}/admin-users?${queryParams}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch admin users');
    }
    return response.json();
  }

  async getAdminUserById(userId: string): Promise<AdminUser> {
    const response = await fetch(`${API_BASE_URL}/admin-users/${userId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch admin user');
    }
    return response.json();
  }

  async createAdminUser(data: CreateAdminUserRequest): Promise<AdminUser> {
    const response = await fetch(`${API_BASE_URL}/admin-users`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to create admin user');
    }
    return response.json();
  }

  async updateAdminUser(userId: string, data: UpdateAdminUserRequest): Promise<AdminUser> {
    const response = await fetch(`${API_BASE_URL}/admin-users/${userId}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to update admin user');
    }
    return response.json();
  }

  async deleteAdminUser(userId: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/admin-users/${userId}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete admin user');
    }
  }

  async authenticateAdminUser(userId: string): Promise<AdminUser> {
    const response = await fetch(`${API_BASE_URL}/admin-users/${userId}/authenticate`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to authenticate admin user');
    }
    return response.json();
  }

  async deauthenticateAdminUser(userId: string): Promise<AdminUser> {
    const response = await fetch(`${API_BASE_URL}/admin-users/${userId}/deauthenticate`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to deauthenticate admin user');
    }
    return response.json();
  }

  async isAuthenticated(userId: string): Promise<boolean> {
    const response = await fetch(`${API_BASE_URL}/admin-users/${userId}/is-authenticated`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to check authentication status');
    }
    return response.json();
  }

  async getAuthenticatedAdminUsers(): Promise<AdminUser[]> {
    const response = await fetch(`${API_BASE_URL}/admin-users/authenticated`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch authenticated admin users');
    }
    return response.json();
  }

  async getUnauthenticatedAdminUsers(): Promise<AdminUser[]> {
    const response = await fetch(`${API_BASE_URL}/admin-users/unauthenticated`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch unauthenticated admin users');
    }
    return response.json();
  }

  async deauthenticateAllAdminUsers(): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/admin-users/deauthenticate-all`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to deauthenticate all admin users');
    }
  }
}

export const adminUserService = new AdminUserService();
