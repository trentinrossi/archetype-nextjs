import { User, CreateUserRequest, UpdateUserRequest, UserListResponse, UserResponse, UserDeleteResponse } from '@/types/user';
import { authService } from './authService';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_URL || '/api';

class UserService {
  private getAuthHeaders(): Record<string, string> {
    const token = authService.getAuthToken();
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getUsers(page: number = 0, size: number = 10, startUserId?: string): Promise<UserListResponse> {
    const params = new URLSearchParams({
      page: page.toString(),
      size: size.toString(),
    });

    if (startUserId) {
      params.append('startUserId', startUserId);
    }

    const response = await fetch(`${API_BASE_URL}/users?${params.toString()}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('Users not found');
      }
      if (response.status === 400) {
        const errorData = await response.json();
        throw new Error(errorData.message || 'Bad request');
      }
      throw new Error('Failed to fetch users');
    }

    return response.json();
  }

  async getUserById(userId: string): Promise<User> {
    const response = await fetch(`${API_BASE_URL}/users/${userId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      if (response.status === 404) {
        const errorData = await response.json();
        throw new Error(errorData.message || 'User ID NOT found...');
      }
      throw new Error('Failed to fetch user');
    }

    return response.json();
  }

  async createUser(data: CreateUserRequest): Promise<UserResponse> {
    const response = await fetch(`${API_BASE_URL}/users`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });

    if (!response.ok) {
      if (response.status === 400) {
        const errorData = await response.json();
        throw new Error(errorData.message || 'User ID already exist...');
      }
      throw new Error('Failed to create user');
    }

    return response.json();
  }

  async updateUser(userId: string, data: UpdateUserRequest): Promise<UserResponse> {
    const response = await fetch(`${API_BASE_URL}/users/${userId}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });

    if (!response.ok) {
      if (response.status === 404) {
        const errorData = await response.json();
        throw new Error(errorData.message || 'User ID NOT found...');
      }
      if (response.status === 400) {
        const errorData = await response.json();
        throw new Error(errorData.message || 'Please modify to update...');
      }
      throw new Error('Failed to update user');
    }

    return response.json();
  }

  async deleteUser(userId: string): Promise<UserDeleteResponse> {
    const response = await fetch(`${API_BASE_URL}/users/${userId}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      if (response.status === 404) {
        const errorData = await response.json();
        throw new Error(errorData.message || 'User ID NOT found...');
      }
      throw new Error('Failed to delete user');
    }

    return response.json();
  }
}

export const userService = new UserService();
